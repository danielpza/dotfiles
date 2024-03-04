{ pkgs, ... }: {
  home.packages = with pkgs; [
    gh
    github-desktop
    slack
    teams-for-linux
    vscodium

    docker
    google-cloud-sdk

    robo3t
    mongodb-compass

    obs-studio

    bruno
  ];
}
