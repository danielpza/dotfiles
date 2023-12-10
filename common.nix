{ config, pkgs, lib, ... }:
let
  userfullname = "Daniel Perez Alvarez";
  username = "daniel";
  useremail = "danielpza@protonmail.com";
  homedir = "/home/${username}";

  extraProfile = ''
    export PATH="$VOLTA_HOME/bin:$PATH"
    # mss https://nodkz.github.io/mongodb-memory-server/docs/api/config-options/
    # export MONGOMS_DOWNLOAD_URL="https://fastdl.mongodb.org/linux/mongodb-linux-x86_64-ubuntu2204-5.0.18.tgz"
    export MONGOMS_DISTRO="ubuntu2204"
  '';

  gnomeExtensions = with pkgs.gnomeExtensions; [
    dash-to-panel
    # material-shell
    # github-notifications
    appindicator
    caffeine
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

  programs.eza = {
    enable = true;
    extraOptions = [ "--group-directories-first" ];
  };
  home.shellAliases = {
    "ls" = "eza";
    "ll" = "eza -la";
    "l" = "eza -a";
  };

  programs.bat.enable = true;
  home.shellAliases.cat = "bat -p";

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    settings = {
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

  home.packages = (with pkgs; [
    # apps
    gnome-feeds
    helix
    keepassxc
    libreoffice
    logseq
    signal-desktop
    spotify
    neovim
    # web dev
    nodejs
    yarn
    # command line helpers
    fd # faster find
    ripgrep # faster grep
    tldr
    dprint # code formatter
    gdtoolkit # godot code formatter
    stylua # lua formatter
    hyperfine
    onefetch
    protonvpn-gui
    yq # yaml/xml/toml processor
    jq # json processor
    github-copilot-cli
    # others
    terraform-ls # terraform language server
    marksman # markdown language server
    terraform # editor support for terraform
    enchant
    hunspell
    hunspellDicts.en_US
    shfmt
    # lua
    luajitPackages.lua-lsp
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
  ]) ++ gnomeExtensions;

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
        feature-mode # cucumber
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
        gdscript-mode
        lua-mode
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
    "hypr" = {
      source = ./hyprland;
      recursive = true;
    };
    "awesome" = {
      source = ./awesome;
      recursive = true;
    };
    "river" = {
      source = ./river;
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
    "pq" = "yarn dlx -p prettier -p pretty-quick pretty-quick";
    "ncu" = "ncu -i --format=group";
  };

  services.flameshot = {
    enable = true;
    settings = {
      General = {
        showDesktopNotification = false;
        saveAfterCopy = true;
      };
    };
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
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

    # Gnome Extensions https://github.com/nix-community/home-manager/issues/284#issuecomment-1321199263
    "org/gnome/shell".enabled-extensions =
      map (extension: extension.extensionUuid) gnomeExtensions;
  };
}
