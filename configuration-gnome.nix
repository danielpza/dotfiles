{ pkgs, homeUsername, ... }: {
  # Enable the GNOME Desktop Environment. https://nixos.wiki/wiki/GNOME
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  environment.gnome.excludePackages = with pkgs; [ gnome-tour ];
  home-manager.users.${homeUsername}.imports = [ ./home-gnome.nix ];
}
