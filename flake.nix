{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { nixpkgs, home-manager, emacs-overlay, ... }:
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
      };
      homeConfigurations.c2 = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./common.nix ./c2.nix ];
      };
      nixosConfigurations.c1 = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [ ./c1-configuration.nix ];
      };
    };
}
