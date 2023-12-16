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
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [ emacs-overlay.overlay ];
      };
    in {
      homeConfigurations.c1 = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./common.nix ./c1.nix ];
        extraSpecialArgs.configName = "c1";
      };
      homeConfigurations.c2 = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./common.nix ./c2.nix ];
        extraSpecialArgs.configName = "c2";
      };
      nixosConfigurations.c1 = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [ ./c1-configuration.nix ];
      };
      nixosConfigurations.c2 = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          nixos-hardware.nixosModules.lenovo-thinkpad-p16s-amd-gen1
          ./configuration-common.nix
          ./c2-configuration.nix
        ];
      };
    };
}
