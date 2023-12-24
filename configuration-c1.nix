# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, configName, homeUsername, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Enable networking
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.backend = "iwd";

  # WMs/Compositors
  # awesome WM
  services.xserver.windowManager.awesome.enable = true;
  # hyprland https://wiki.hyprland.org/Nix/
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
    # enableNvidiaPatches = true;
  };
  # services.xserver.windowManager.dwm.enable = true;
  # services.xserver.windowManager.dwm.package = pkgs.dwm.overrideAttrs {
  #   # use https://github.com/danielpza/dwm
  #   # src = pkgs.fetchFromGitHub {
  #   #   owner = "danielpza";
  #   #   repo = "dwm";
  #   #   rev = "004cf2676d5aaaf5a66461ccdfba7767907bdcc4";
  #   #   sha256 = "sha256-CQkxvsuXuRxPluMmDvAtaLD/C2cKak9cHPRIsLezjJY=";
  #   # };
  #   src = /home/daniel/projects/dwm;
  # };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  home-manager.users.${homeUsername} = import ./home-c1.nix;

  # nix = {
  #   package = pkgs.nixFlakes;
  #   settings = {
  #     max-jobs = 8;
  #     experimental-features = [ "nix-command" "flakes" ];
  #   };
  # };

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Setup keyfile
  boot.initrd.secrets = { "/crypto_keyfile.bin" = null; };

  # Enable swap on luks
  boot.initrd.luks.devices."luks-4cbaf115-d3a9-4356-9eae-841a2ccf9612".device =
    "/dev/disk/by-uuid/4cbaf115-d3a9-4356-9eae-841a2ccf9612";
  boot.initrd.luks.devices."luks-4cbaf115-d3a9-4356-9eae-841a2ccf9612".keyFile =
    "/crypto_keyfile.bin";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
