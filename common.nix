{ config, pkgs, lib, ... }:
let
  userfullname = "Daniel Perez Alvarez";
  username = "daniel";
  useremail = "danielpza@protonmail.com";
  homedir = "/home/${username}";
in {
  imports = lib.optional (builtins.pathExists ./personal/personal.nix)
    ./personal/personal.nix;

  nixpkgs.config.allowUnfree = true;

  # TL;DR: don't touch this line
  home.stateVersion = "22.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.username = username;
  home.homeDirectory = homedir;

  programs.exa = {
    enable = true;
    extraOptions = [ "--group-directories-first" ];
  };
  home.shellAliases = {
    "ls" = "exa";
    "ll" = "exa -la";
    "l" = "exa -a";
  };

  programs.bat.enable = true;
  home.shellAliases.cat = "bat";

  programs.bash.enable = true;
  programs.bash.bashrcExtra = ''
    PS1='\[\e[0;2m\]$? \[\e[0;1;32m\]\w\[\e[0;1;32m\]\$ \[\e[0m\]'
  '';

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];

  home.packages = (with pkgs; [
    # apps
    firefox
    keepassxc
    libreoffice
    logseq
    helix
    # command line helpers
    fd # faster find
    ripgrep # faster grep
    tldr
    dprint # code formatter
    # others
    enchant
    hunspell
    hunspellDicts.en_US
    # web dev
    nodejs
    yarn
    # nix:
    nil # nix lsp
    nixfmt # nix formatter
  ]) ++ (with pkgs.nodePackages_latest; [
    npm-check-updates
    prettier
    typescript
    typescript-language-server
    vscode-langservers-extracted
    yaml-language-server
  ]);

  programs.zoxide = {
    enable = true;
    options = [ "--cmd" "j" ];
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-unstable;
    extraPackages = epkgs:
      with epkgs;
      [
        apheleia
        consult
        consult-flycheck
        consult-lsp
        corfu
        diff-hl
        dirvish
        doom-modeline
        doom-themes
        editorconfig
        ef-themes
        evil
        evil-collection
        evil-indent-plus
        flycheck
        git-link
        git-modes
        jinx
        kind-icon
        lsp-mode
        magit
        markdown-mode
        nerd-icons
        nerd-icons-completion
        nix-mode
        orderless
        protobuf-mode
        treesit-grammars.with-all-grammars
        vertico
        which-key
        yasnippet
      ] ++ [
        # non elpa/melpa
        (callPackage ./emacs/copilot.el.nix {
          inherit (pkgs) fetchFromGitHub;
          inherit (epkgs) trivialBuild s dash editorconfig;
        })
      ];
  };

  xdg.configFile = {
    "helix" = {
      source = ./helix;
      recursive = true;
    };
    "dprint" = {
      source = ./dprint;
      recursive = true;
    };
    "emacs" = {
      source = ./emacs;
      recursive = true;
    };
  };

  programs.git = {
    enable = true;
    userName = userfullname;
    userEmail = useremail;
    extraConfig = { init.defaultBranch = "master"; };
  };

  home.sessionVariables = {
    EDITOR = "emacs --alternate-editor=";

    # make other programs play nice with xdg https://wiki.archlinux.org/title/XDG_Base_Directory
    BUN_INSTALL =
      "${config.xdg.dataHome}/.bun"; # https://github.com/oven-sh/bun/issues/696
    GOPATH = "${config.xdg.dataHome}/go";
    PYENV_ROOT = "${config.xdg.dataHome}/pyenv";
    VOLTA_HOME = "${config.xdg.dataHome}/volta";
    SSB_HOME = "${config.xdg.dataHome}/zoom"; # zoom app

    # pnpm
    PNPM_HOME = "${config.xdg.dataHome}/pnpm";

    # npm https://github.com/npm/rfcs/issues/389#issuecomment-871656832
    npm_config_userconfig = "${config.xdg.configHome}/npm/config";
    npm_config_cache = "${config.xdg.cacheHome}/npm";
    npm_config_prefix = "${config.xdg.dataHome}/npm";
    NODE_REPL_HISTORY = "${config.xdg.dataHome}/node_repl_history";

    # gtk 1&2
    GTK_RC_FILES = "${config.xdg.configHome}/gtk-1.0/gtkrc";
    GTK2_RC_FILES = "${config.xdg.configHome}/gtk-2.0/gtkrc";

    # kde
    KDEHOME = "${config.xdg.configHome}/kde";
  };

  home.sessionPath = [
    "$BUN_INSTALL/bin"
    "$GOPATH/bin"
    "$PYENV_ROOT/bin"
    "$VOLTA_HOME/bin"
    "$PNPM_HOME/bin"
    "$npm_config_prefix/bin"
  ];

  home.shellAliases = {
    "pq" = "pretty-quick";
    "ncu" = "ncu -i --format=group";
    "hm" = "home-manager switch";
  };

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
