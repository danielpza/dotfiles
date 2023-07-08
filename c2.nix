{ pkgs, ... }: {
  imports = [ ./common.nix ];

  home.packages = with pkgs; [
    signal-desktop
    firefox-devedition
    gh
    github-desktop
    slack
    teams-for-linux
    volta
    vscodium
    zoom-us
  ];

  home.sessionVariables = {
    QT_XCB_GL_INTEGRATION =
      "none"; # fix for zoom-us issue on nixos https://github.com/NixOS/nixpkgs/issues/82959
  };

  targets.genericLinux.enable = true;

  nixpkgs.config.permittedInsecurePackages = [ "openssl-1.1.1u" ];
}
