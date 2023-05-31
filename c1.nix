{ pkgs, ... }: {
  imports = [ ./common.nix ];

  home.packages = with pkgs; [
    signal-desktop
    spotify
    slack
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
