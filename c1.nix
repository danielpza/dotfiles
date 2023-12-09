{ pkgs, ... }: {
  home.packages = with pkgs; [ slack firefox ];

  home.shellAliases = {
    "hm" =
      "home-manager switch --flake path:/home/daniel/.config/home-manager#c1";
  };

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      monospace-font-name = "Source Code Pro 20";
    };
  };
}
