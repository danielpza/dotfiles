{ config, pkgs, ... }:
let
  userfullname = "Daniel Perez Alvarez";
  username = "daniel";
  useremail = "danielpza@protonmail.com";
  homedir = "/home/${username}";
  pkgsUnstable = import <nixpkgs-unstable> { };
in {
  nixpkgs.config.allowUnfree = true;

  # TL;DR: don't touch this line
  home.stateVersion = "22.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.username = username;
  home.homeDirectory = homedir;

  programs.bash.enable = true;

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];

  home.packages = with pkgs; [
    # apps
    firefox
    keepassxc
    pkgsUnstable.logseq
    signal-desktop
    slack
    spotify
    # others
    appimage-run # for AppImage
    ispell # for emacs
    # command line helpers
    fd
    ripgrep
    tldr
    # web dev
    nodejs
    yarn
    # node packages
    pkgsUnstable.nodePackages.prettier
    pkgsUnstable.nodePackages.typescript
    pkgsUnstable.nodePackages.typescript-language-server
    pkgsUnstable.nodePackages.vscode-langservers-extracted
    # nix:
    nil # nix lsp
    nixfmt # nix formatter
  ];

  programs.zoxide = {
    enable = true;
    options = [ "--cmd" "j" ];
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsUnstable;
    extraPackages = epkgs:
      with epkgs; [
        all-the-icons
        all-the-icons-completion
        apheleia
        consult
        corfu
        diff-hl
        doom-modeline
        editorconfig
        evil
        evil-collection
        evil-indent-plus
        kind-icon
        lsp-mode
        magit
        markdown-mode
        nix-mode
        orderless
        treemacs
        treemacs-all-the-icons
        treemacs-evil
        treemacs-magit
        vertico
        which-key
      ];
  };

  home.file = {
    ".config/emacs/" = {
      source = ./emacs;
      recursive = true;
    };
  };

  programs.git = {
    enable = true;
    userName = userfullname;
    userEmail = useremail;
  };

  home.sessionVariables = { EDITOR = "emacs --alternate-editor="; };

  # home.keyboard.options = [ "caps:escape" ];

  # GNOME configuration
  dconf.settings = {
    "org/gnome/desktop/interface" = { show-battery-percentage = true; };
    "org/gnome/desktop/input-sources" = { xkb-options = [ "caps:escape" ]; };
    "org/gtk/gtk4/settings/file-chooser" = { sort-directories-first = true; };
    "org/gnome/settings-daemon/plugins/color" = {
      night-light-enabled = true;
      night-light-schedule-automatic = true;
    };
  };
}
