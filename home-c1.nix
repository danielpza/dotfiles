{ pkgs, pkgs-unstable, ... }: {
  home.packages = with pkgs;
    [ protonvpn-cli freetube godot_4 thunderbird standardnotes protonvpn-gui ]
    ++ (with pkgs-unstable; [
      # protonvpn-gui
      protonmail-desktop
      libsecret
    ]);
}
