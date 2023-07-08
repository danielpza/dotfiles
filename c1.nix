{ pkgs, ... }: {
  imports = [ ./common.nix ];

  home.packages = with pkgs; [
    gnome-feeds
    signal-desktop
    spotify
    slack
    firefox
    # webdev
    nodejs
    yarn
  ];

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      monospace-font-name = "Source Code Pro 20";
    };
  };
}
