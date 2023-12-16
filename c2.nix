{ pkgs, ... }: {
  home.packages = with pkgs; [
    firefox-devedition
    gh
    github-desktop
    slack
    teams-for-linux
    vscodium

    # libnotify
    # solaar

    # earthly
    docker
    google-cloud-sdk

    robo3t
    mongodb-compass

    obs-studio

    bruno
  ];
}
