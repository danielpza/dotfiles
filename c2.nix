{ pkgs, ... }: {
  imports = [ ./common.nix ];

  home.packages = with pkgs; [ github-desktop slack ];

  programs.bash.bashrcExtra = ''
    . "$HOME/xdg-base-directory-fix.sh"
    . "/home/daniel/.local/share/cargo/env"
  '';

  targets.genericLinux.enable = true;
}
