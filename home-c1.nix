{ pkgs, ... }: {
  home.packages = with pkgs; [
    freetube
    godot_4
    protonmail-desktop
    protonvpn-cli
    protonvpn-gui
    slack
    thunderbird
  ];
}
