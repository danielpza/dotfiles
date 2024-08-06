{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-2311.url = "github:nixos/nixpkgs/nixos-23.11";
    nixos-hardware.url = "github:nixos/nixos-hardware/master";
    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs-unstable";
  };
  outputs = { nixpkgs, nixpkgs-unstable, nixpkgs-2311, home-manager
    , emacs-overlay, nixos-hardware, ... }:
    let
      system = "x86_64-linux";
      overlays = [ emacs-overlay.overlay ];
      overlays-config = { nixpkgs.overlays = overlays; };
      pkgs = import nixpkgs { inherit system; };
      pkgs-unstable = import nixpkgs-unstable { inherit system overlays; };
      pkgs-2311 = import nixpkgs-2311 { inherit system overlays; };
      username = "daniel";
    in {
      homeConfigurations.c1 = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./home.nix ./home-c1.nix ./home-gnome.nix ];
        extraSpecialArgs.configName = "c1";
        extraSpecialArgs = {
          inherit pkgs-unstable;
          inherit pkgs-2311;
        };
      };
      homeConfigurations.c2 = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./home.nix ./home-c2.nix ./home-gnome.nix ];
        extraSpecialArgs.configName = "c2";
      };
      nixosConfigurations.c1 = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs.configName = "c1";
        specialArgs.homeUsername = username;
        modules = [
          # home-manager.nixosModules.home-manager
          overlays-config
          ./configuration.nix
          ./configuration-c1.nix
        ];
      };
      nixosConfigurations.c2 = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs.configName = "c2";
        specialArgs.homeUsername = username;
        modules = [
          # home-manager.nixosModules.home-manager
          overlays-config
          # nixos-hardware.nixosModules.lenovo-thinkpad-p16s-amd-gen1
          ./configuration.nix
          ./configuration-c2.nix
        ];
      };
    };
}
