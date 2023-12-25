{ pkgs, ... }: {

  home.packages = with pkgs; [ gnome-feeds ];

  # GNOME configuration
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      show-battery-percentage = true;
      enable-hot-corners = false;
      monospace-font-name = "Source Code Pro 20";
    };
    "org/gnome/desktop/input-sources" = { xkb-options = [ "caps:escape" ]; };
    "org/gtk/gtk4/settings/file-chooser" = { sort-directories-first = true; };
    "org/gnome/settings-daemon/plugins/color" = {
      night-light-enabled = false;
      night-light-schedule-automatic = true;
    };
    "org/gnome/desktop/session" = { idle-delay = 0; };
    "org/gnome/settings-daemon/plugins/power" = {
      power-button-action = "hibernate";
    };

    # https://the-empire.systems/nixos-gnome-settings-and-keyboard-shortcuts
    # https://discourse.nixos.org/t/nixos-options-to-configure-gnome-keyboard-shortcuts/7275
    "org/gnome/shell/keybindings" = {
      toggle-application-view = [ ];
      toggle-overview = [ ];
      show-screenshot-ui = [ "<Super>R" ];
      show-desktop = [ "<Super>D>" ];
    };
    "org/gnome/settings-daemon/plugins/media-keys" = {
      help = [ ];
      custom-keybindings = [
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/"
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/"
      ];
    };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" =
      {
        name = "flameshot";
        command = "flameshot gui";
        binding = "<Super>S";
      };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1" =
      {
        name = "restart wifi";
        command = "sh ${./bin/restart-wifi.sh}";
        binding = "<Super>C";
      };
    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2" =
      {
        name = "reconnect bluetooth";
        command = "sh ${./bin/reconnect-bluetooth}";
        binding = "<Super>F";
      };
    "org/gnome/shell" = {
      favorite-apps = [
        "emacsclient.desktop"
        "org.gnome.Console.desktop"
        "firefox.desktop"
        "slack.desktop"
        "org.gnome.Nautilus.desktop"
        "logseq.desktop"
      ];
    };

    # Gnome Extensions https://github.com/nix-community/home-manager/issues/284#issuecomment-1321199263
  };
}
