{ pkgs, ... }: {
  imports = [ ./common.nix ];

  home.packages = with pkgs; [
    signal-desktop
    spotify
    slack
    # webdev
    nodejs
    yarn
  ];
}
