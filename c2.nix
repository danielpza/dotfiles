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
    docker
    google-cloud-sdk

    robo3t
    mongodb-compass

    obs-studio

    bruno
  ];

  home.shellAliases = {
    "hm" = "home-manager switch --flake path:${configDir}#${config}";
    "nu" = "nix flake update ${configDir}";
    "nr" = "sudo nixos-rebuild switch --flake path:${configDir}#${config}";
  };

  nixpkgs.config.permittedInsecurePackages = [ "electron-25.9.0" ];

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      monospace-font-name = "Source Code Pro 20";
    };
  };
}
