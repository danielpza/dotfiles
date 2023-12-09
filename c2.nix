{ pkgs, ... }: {
  home.packages = with pkgs; [
    firefox-devedition
    gh
    github-desktop
    slack
    teams-for-linux
    vscodium
    zoom-us
    docker
    google-cloud-sdk

    obs-studio
  ];

  home.shellAliases = {
    "hm" =
      "home-manager switch --flake path:/home/daniel/.config/home-manager#c2";
  };

  nixpkgs.config.permittedInsecurePackages = [ "openssl-1.1.1w" ];

  dconf.settings = {
    "org/gnome/desktop/interface" = {
      monospace-font-name = "Source Code Pro 20";
    };
  };
}
