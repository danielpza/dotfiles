# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    #<home-manager/nixos>
  ];
  programs.nix-ld.enable =
    true; # https://github.com/Mic92/nix-ld and https://discourse.nixos.org/t/how-to-link-linux-vdso/22965/6

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    jack.enable = true;
    # package = pkgs.pipewire_0_2;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.daniel = {
    isNormalUser = true;
    description = "Daniel Perez";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    # packages = with pkgs;
    #   [
    #      firefox
    #      thunderbird
    #   ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [ logitech-udev-rules solaar ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

  # https://nixos.wiki/wiki/Nvidia
  # Make sure opengl is enabled
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  # NVIDIA drivers are unfree.
  # nixpkgs.config.allowUnfreePredicate = pkg:
  #   builtins.elem (lib.getName pkg) [ "nvidia-x11" "nvidia-settings" ];
  # services.xserver.videoDrivers = [ "nvidia" ];
  # hardware.nvidia = {
  #   # Modesetting is needed for most wayland compositors
  #   modesetting.enable = true;
  #   # Use the open source version of the kernel module
  #   # Only available on driver 515.43.04+
  #   open = true;
  #   # Enable the nvidia settings menu
  #   nvidiaSettings = true;
  #   # Optionally, you may need to select the appropriate driver version for your specific GPU.
  #   # package = config.boot.kernelPackages.nvidiaPackages.stable;
  # };

  virtualisation.docker.enable = true; # https://nixos.wiki/wiki/Docker
  virtualisation.docker.rootless = {
    enable = true;
    setSocketVariable = true;
  };

  hardware.opengl.extraPackages = with pkgs; [
    # https://nixos.org/manual/nixos/stable/index.html#sec-gpu-accel
    intel-compute-runtime
    rocm-opencl-icd
  ];

  programs.sway.enable =
    true; # https://nixos.org/manual/nixos/unstable/index.html#sec-wayland
  xdg.portal.wlr.enable = true;
  # xdg = {
  #   portal = {
  #     enable = true;
  #     extraPortals = with pkgs;
  #       [
  #         xdg-desktop-portal-wlr
  #         # xdg-desktop-portal-gtk
  #       ];
  #     gtkUsePortal = true;
  #   };
  # };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
}
