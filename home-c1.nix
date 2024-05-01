{ pkgs, pkgs-unstable, ... }: {
  home.packages = with pkgs;
    [ protonvpn-cli freetube godot_4 thunderbird ]
    ++ (with pkgs-unstable; [ protonvpn-gui protonmail-desktop ]);
}
