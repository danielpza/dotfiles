{ pkgs, ... }: {
  imports = [ ./common.nix ];

  home.packages = with pkgs; [ github-desktop slack ];

  targets.genericLinux.enable = true;
}
