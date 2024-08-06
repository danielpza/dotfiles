# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, configName, homeUsername, ... }:

{
  imports = [ ./hardware-configuration.nix ./configuration-gnome.nix ];

  # home-manager.users.${homeUsername} = import ./home-c1.nix;

  # WMs/Compositors
  services.xserver.windowManager.awesome.enable = true;
  programs.river.enable = true;
  programs.hyprland = {
    # https://wiki.hyprland.org/Nix/
    enable = true;
    xwayland.enable = true;
    # enableNvidiaPatches = true;
  };

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

  system.stateVersion = "22.11"; # TL;DR: don't touch this line

  environment.systemPackages = [ pkgs.solaar ];
}
