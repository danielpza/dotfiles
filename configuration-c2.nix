# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, homeUsername, ... }:

{
  imports = [ ./hardware-configuration.nix ./configuration-gnome.nix ];

  # home-manager.users.${homeUsername} = import ./home-c2.nix;
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  virtualisation.docker.enable = true; # https://nixos.wiki/wiki/Docker

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  programs.steam = {
    enable = true;
    remotePlay.openFirewall =
      true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall =
      true; # Open ports in the firewall for Source Dedicated Server
  };

  system.stateVersion = "23.05"; # TL;DR: don't touch this line

  services.xserver.videoDrivers = [
    # "amdgpu"
    # "modesetting"
    "nvidia"
    # "nvidiaLegacy470"
  ];

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  hardware.nvidia = {

    # Modesetting is required.
    modesetting.enable = true;

    # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
    powerManagement.enable = false;
    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    powerManagement.finegrained = false;

    # Use the NVidia open source kernel module (not to be confused with the
    # independent third-party "nouveau" open source driver).
    # Support is limited to the Turing and later architectures. Full list of
    # supported GPUs is at:
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
    # Only available from driver 515.43.04+
    # Currently alpha-quality/buggy, so false is currently the recommended setting.
    open = false;

    # Enable the Nvidia settings menu,
    # accessible via `nvidia-settings`.
    nvidiaSettings = true;

    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    package = config.boot.kernelPackages.nvidiaPackages.stable;
    # package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  nixpkgs.config.nvidia.acceptLicense = true; # sell your soul

  hardware.nvidia.prime = {
    # Make sure to use the correct Bus ID values for your system!
    # intelBusId = "PCI:0:2:0";
    # nvidiaBusId = "PCI:01:0:0";
  };

  system.nixos.label = "gaming";
}
