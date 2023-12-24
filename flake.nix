{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware/master";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { nixpkgs, home-manager, emacs-overlay, nixos-hardware, ... }:
    let
      system = "x86_64-linux";
      overlays = [ emacs-overlay.overlay ];
      overlays-config = { nixpkgs.overlays = overlays; };
      username = "daniel";
    in {
      nixosConfigurations.c1 = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs.configName = "c1";
        specialArgs.homeUsername = username;
        modules = [
          home-manager.nixosModules.home-manager
          overlays-config
          ./configuration.nix
          ./configuration-c1.nix
          ./configuration-gnome.nix
        ];
      };
      nixosConfigurations.c2 = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs.configName = "c1";
        specialArgs.homeUsername = username;
        modules = [
          home-manager.nixosModules.home-manager
          nixos-hardware.nixosModules.lenovo-thinkpad-p16s-amd-gen1
          ./configuration.nix
          ./configuration-c2.nix
          ./configuration-gnome.nix
        ];
      };
    };
}
