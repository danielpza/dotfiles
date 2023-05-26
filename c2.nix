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

  nixpkgs.config.permittedInsecurePackages = [ "openssl-1.1.1t" ];
}
