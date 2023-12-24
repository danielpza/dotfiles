# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, homeUsername, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  home-manager.users.${homeUsername} = import ./home-c2.nix;

  virtualisation.docker.enable = true; # https://nixos.wiki/wiki/Docker

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  system.stateVersion = "23.05"; # TL;DR: don't touch this line
}
