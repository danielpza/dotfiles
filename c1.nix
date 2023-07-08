{ pkgs, ... }: {
  imports = [ ./common.nix ];

  home.packages = with pkgs; [ slack firefox ];

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      monospace-font-name = "Source Code Pro 20";
    };
  };
}
