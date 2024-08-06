{ pkgs, pkgs-unstable, pkgs-2311, lib, ... }: {
  home.packages = with pkgs;
    [
      freetube
      # godot_4
      # thunderbird
    ] ++ (with pkgs-unstable; [
      protonvpn-gui
      protonvpn-cli
      proton-pass
      (lib.hiPrio protonmail-desktop)
    ]) ++ (with pkgs-2311;
      [
        standardnotes # latest version not working but 2311 ones does
      ]);
}
