{ pkgs, ... }: {
  imports = [ ./common.nix ];

  home.packages = with pkgs; [
    firefox-devedition
    gh
    github-desktop
    slack
    teams-for-linux
    vscodium
    zoom-us
  ];

  nixpkgs.config.permittedInsecurePackages = [ "openssl-1.1.1u" ];

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      monospace-font-name = "Source Code Pro 20";
    };
  };
}
