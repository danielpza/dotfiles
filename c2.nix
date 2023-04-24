{ pkgs, ... }: {
  imports = [ ./common.nix ];

  home.packages = with pkgs; [
    github-desktop
    slack
    teams-for-linux
    gh
    obs-studio
  ];

  targets.genericLinux.enable = true;
}
