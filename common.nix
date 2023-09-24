{ config, pkgs, lib, ... }:
let
  userfullname = "Daniel Perez Alvarez";
  username = "daniel";
  useremail = "danielpza@protonmail.com";
  homedir = "/home/${username}";

  extraProfile = ''
    export PATH="$VOLTA_HOME/bin:$PATH"
    export LD_LIBRARY_PATH="${
      lib.makeLibraryPath (with pkgs; [
        stdenv.cc.cc
        openssl
        # openssl_1_1 # https://discourse.nixos.org/t/how-to-fix-library-is-missing-or-cannot-be-opened-libcrypto-so-1-1/30730
        # lzlib # related https://github.com/NixOS/nix/issues/1550
        libGL
        libuuid
        # curlFull
      ])
    }:$LD_LIBRARY_PATH"
  '';

  gnomeExtensions = with pkgs.gnomeExtensions;
    [
      dash-to-panel
      # material-shell
    ];
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

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    settings = {
      gcloud.disabled = true;
      docker_context.disabled = true;
      buf.disabled = true;
      git_branch.disabled = true;
      nodejs.disabled = true;
    };
  };

  programs.bash.enable = true;
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableAutosuggestions = true;
    syntaxHighlighting.enable = true;
    initExtra = ''
      bindkey "^[[1;5C" forward-word
      bindkey "^[[1;5D" backward-word
    '';
  };

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];

  home.packages = (with pkgs; [
    # apps
    gnome-feeds
    helix
    keepassxc
    libreoffice
    logseq
    signal-desktop
    spotify
    # web dev
    volta
    # command line helpers
    fd # faster find
    ripgrep # faster grep
    tldr
    dprint # code formatter
    hyperfine
    onefetch
    # others
    terraform-ls # terraform language server
    marksman # markdown language server
    terraform # editor support for terraform
    enchant
    hunspell
    hunspellDicts.en_US
    shfmt
    # nix:
    nil # nix lsp
    nixfmt # nix formatter
  ]) ++ (with pkgs.nodePackages_latest; [
    npm-check-updates
    # prettier
    typescript
    typescript-language-server
    vscode-langservers-extracted
    yaml-language-server
    bash-language-server
    dockerfile-language-server-nodejs
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
        lsp-ui
        lsp-pyright
        magit
        markdown-mode
        nerd-icons
        nerd-icons-completion
        nix-mode
        orderless
        protobuf-mode
        terraform-mode
        treesit-grammars.with-all-grammars
        vertico
        which-key
        yasnippet
        earthfile-mode # https://github.com/earthly/earthly-emacs
        embark
        embark-consult
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

    NIXOS_OZONE_WL = "1"; # https://nixos.wiki/wiki/Slack

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

    # others
    CALCHISTFILE = "${config.xdg.cacheHome}/calc_history";
    ASPELL_CONF =
      "${config.xdg.configHome}/aspell/aspell.conf; personal ${config.xdg.configHome}/aspell/en.pws; repl ${config.xdg.configHome}/aspell/en.prepl";

    # volta fix
    # https://discourse.nixos.org/t/node2nix-issues/10762/2
    NIX_LD_LIBRARY_PATH =
      lib.makeLibraryPath (with pkgs; [ stdenv.cc.cc openssl libuuid ]);
    NIX_LD = lib.fileContents "${pkgs.stdenv.cc}/nix-support/dynamic-linker";

    # https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization)
    # LSP_USE_PLISTS = "true";

    MOZ_USE_XINPUT2 = "1";
  };

  programs.bash.bashrcExtra = extraProfile;
  programs.zsh.profileExtra = extraProfile;

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
    "hmu" = "nix-channel --update && home-manager switch";
    "nu" = "sudo nix-channel --update && sudo nixos-rebuild switch";
    "nr" = "sudo nixos-rebuild switch";
    "nedit" = "sudo -E emacs /etc/nixos/configuration.nix";
  };

  # home.keyboard.options = [ "caps:escape" ];

  # GNOME configuration
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      show-battery-percentage = true;
      enable-hot-corners = false;
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

    # Gnome Extensions https://github.com/nix-community/home-manager/issues/284#issuecomment-1321199263
    "org/gnome/shell".enabled-extensions =
      map (extension: extension.extensionUuid) gnomeExtensions;
  };
}
