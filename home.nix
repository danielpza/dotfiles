{ config, pkgs, ... }:
let
  userfullname = "Daniel Perez Alvarez";
  username = "daniel";
  useremail = "danielpza@protonmail.com";
  homedir = "/home/daniel";
  custom-logseq = with pkgs;
    (appimageTools.wrapType2 rec { # logseq
      pname = "logseq";
      version = "0.9.3";
      name = "Logseq";

      src = fetchurl {
        url =
          "https://github.com/logseq/logseq/releases/download/0.9.2/Logseq-linux-x64-${version}.AppImage";
        sha256 =
          "8700534ba8630fae923ce3dcb5c4b67acc9b9d5ae0c3bedf169b7ca611addf7e";
        name = "${pname}-${version}.AppImage";
      };

      # from https://github.com/NixOS/nixpkgs/blob/nixos-22.11/pkgs/applications/misc/logseq/default.nix#L30
      extraInstallCommands =
        let contents = appimageTools.extract { inherit pname version src; };
        in ''
          mkdir -p $out/bin $out/share/${pname} $out/share/applications
          cp -a ${contents}/{locales,resources} $out/share/${pname}
          cp -a ${contents}/Logseq.desktop $out/share/applications/${pname}.desktop
        '';
    });
in {
  nixpkgs.config.allowUnfree = true;

  # TL;DR: don't touch this line
  home.stateVersion = "22.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.username = username;
  home.homeDirectory = homedir;

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];

  home.packages = with pkgs; [
    # apps
    custom-logseq
    firefox
    keepassxc
    signal-desktop
    spotify
    # others
    appimage-run # for AppImage
    ispell # for emacs
    # command line helpers
    tldr
    fd
    ripgrep
    zoxide
    # web dev:
    nodejs
    yarn
    # nix:
    cachix # nix caching
    nil # nix lsp
    nixfmt # nix formatter
  ];

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
