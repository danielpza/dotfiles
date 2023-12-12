{ pkgs, ... }:
let
  configDir = "/home/daniel/.config/home-manager";
  config = "c2";
in {
  home.packages = with pkgs; [
    firefox-devedition
    gh
    github-desktop
    slack
    teams-for-linux
    vscodium
    zoom-us
    docker
    google-cloud-sdk

    obs-studio
  ];

  home.shellAliases = {
    "hm" = "home-manager switch --flake path:${configDir}#${config}";
    "nu" = "nix flake update ${configDir}";
    "nr" = "sudo nixos-rebuild switch --flake path:${configDir}#${config}";
  };

  nixpkgs.config.permittedInsecurePackages = [ "openssl-1.1.1w" ];

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      monospace-font-name = "Source Code Pro 20";
    };
  };
}
