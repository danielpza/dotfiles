{ config, lib, pkgs, configName, homeUsername, ... }: {
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # home-manager = {
  #   useGlobalPkgs = true;
  #   useUserPackages = true;
  #   extraSpecialArgs.configName = configName;
  #   users.${homeUsername} = import ./home.nix;
  # };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # https://github.com/Mic92/nix-ld
  programs.nix-ld.enable = true;

  networking.hostName = "nixos"; # Define your hostname.

  # Enable networking
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.backend = "iwd";

  # more shells
  programs.zsh.enable = true;
  programs.fish.enable = true;
  users.defaultUserShell = pkgs.fish;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.${homeUsername} = {
    isNormalUser = true;
    description = "Daniel Perez";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
  };

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

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.bluetooth.enable = true; # TODO check if needed
  hardware.pulseaudio.enable = false;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  hardware.enableAllFirmware = true; # TODO check if needed
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  nixpkgs.config.permittedInsecurePackages = [ "electron-25.9.0" ];
}
