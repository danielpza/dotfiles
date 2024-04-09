{ config, pkgs, lib, configName, ... }:
let
  userfullname = "Daniel Perez Alvarez";

  username = "daniel";
  useremail = "danielpza@protonmail.com";
  configDir = "/home/${username}/.config/home-manager";
in {
  imports = (lib.optional (builtins.pathExists ./personal/personal.nix)
    ./personal/personal.nix);

  nixpkgs.config.allowUnfree = true;
  # nixpkgs.config.allowUnfreePredicate = _:
  #   true; # https://nixos.wiki/wiki/Unfree_Software

  # TL;DR: don't touch this line
  home.stateVersion = "22.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.username = username;
  home.homeDirectory = "/home/${username}";

  # improved shell experience
  # better ls
  programs.eza = {
    enable = true;
    extraOptions = [ "--group-directories-first" ];
  };
  home.shellAliases = {
    ls = "eza";
    ll = "eza -la";
    l = "eza -a";
    npm = "corepack npm";
    npx = "corepack npx";
  };
  # better cat
  programs.bat.enable = true;
  home.shellAliases.cat = "bat -p";
  # better prompt
  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    enableFishIntegration = true;
    settings = {
      docker_context.disabled = true;
      buf.disabled = true;
      git_branch.disabled = true;
    };
  };
  # autojump/zoxide
  programs.zoxide = {
    enable = true;
    options = [ "--cmd" "j" ];
  };
  # use zsh
  programs.bash.enable = true;
  programs.fish.enable = true;
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
  programs.ripgrep = {
    enable = true;
    arguments = [ "--hidden" ];
  };

  # aliases to rebuild home-manager/nixos
  home.shellAliases = {
    "hm" = "home-manager switch --flake path:${configDir}#${configName}";
    "nu" = "nix flake update ${configDir}";
    "nr" = "sudo nixos-rebuild switch --flake path:${configDir}#${configName}";
    "tddj" = "yarn test -o --watch"; # expects npx jest --watch -o
    "tddv" =
      "yarn test --changed --watch"; # expects npx vitest --watch --changed
  };

  home.packages = (with pkgs; [
    # apps
    helix
    keepassxc
    libreoffice
    logseq
    signal-desktop
    spotify
    neovim
    appimage-run
    # web dev
    (nodejs.override { enableNpm = false; })
    corepack
    oxlint
    # command line helpers
    marp-cli # markdown slides
    tree
    fd # faster find
    tldr
    dprint # code formatter
    gdtoolkit # godot code formatter
    stylua # lua formatter
    hyperfine
    onefetch
    yq # yaml/xml/toml processor
    jq # json processor
    github-copilot-cli
    # others
    bemoji # emoji picker
    # dmenu # needed for bemoji on x11
    rofi # needed for bemoji on x11
    xclip # needed for bemoji on x11
    xdotool # needed for bemoji on x11
    grpcui
    terraform-ls # terraform language server
    marksman # markdown language server
    terraform # editor support for terraform
    enchant
    hunspell
    hunspellDicts.en_US
    shfmt
    black # python formatter
    yapf # python formatter
    # lua
    luajitPackages.lua-lsp
    # nix:
    nil # nix lsp
    nixfmt # nix formatter
    # libs needed? perhaps?
    # curlFull
    nix-index
  ]) ++ (with pkgs.nodePackages_latest; [
    npm-check-updates
    prettier
    typescript
    typescript-language-server
    vscode-langservers-extracted
    yaml-language-server
    bash-language-server
    dockerfile-language-server-nodejs
    pyright
  ]);
  programs.librewolf = {
    enable = true;
    settings = {
      "privacy.resistFingerprinting.letterboxing" = true;
      "privacy.clearOnShutdown.history" = false;
    };
  };

  services.emacs.defaultEditor = true;
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-unstable;
    extraPackages = epkgs:
      with epkgs; [
        apheleia
        consult
        consult-flycheck
        consult-lsp
        corfu
        # company
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
        envrc
        cape
        hl-todo
        copilot
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
  };

  programs.git = {
    enable = true;
    userName = userfullname;
    userEmail = useremail;
    extraConfig = { init.defaultBranch = "master"; };
    ignores = [ ".dir-locals-2.el" ];
    # delta.enable = true;
    diff-so-fancy.enable = true;
  };
  programs.jujutsu = {
    enable = true;
    settings = {
      user.name = userfullname;
      user.email = useremail;
    };
  };

  home.sessionVariables = {
    EDITOR = "emacs --alternate-editor=";

    # NIXOS_OZONE_WL = "1"; # https://nixos.wiki/wiki/Slack

    # make other programs play nice with xdg https://wiki.archlinux.org/title/XDG_Base_Directory
    BUN_INSTALL =
      "${config.xdg.dataHome}/.bun"; # https://github.com/oven-sh/bun/issues/696
    GOPATH = "${config.xdg.dataHome}/go";
    PYENV_ROOT = "${config.xdg.dataHome}/pyenv";
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

  home.sessionPath = [
    "$BUN_INSTALL/bin"
    "$GOPATH/bin"
    "$PYENV_ROOT/bin"
    "$PNPM_HOME/bin"
    "$npm_config_prefix/bin"
  ];

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

  # programs.waybar.enable = true;

  nixpkgs.config.permittedInsecurePackages = [ "electron-25.9.0" ];

  xdg.mimeApps = let
    editor = "emacsclient.desktop";
    pdfviewer = "org.gnome.Evince.desktop";
    associations = {
      "text/plain" = [ editor ];
      "text/org" = [ editor ];
      "text/markdown" = [ editor ];
      "application/json" = [ editor ];
      "application/pdf" = [ pdfviewer ];
    };
  in {
    enable = true;
    associations.added = associations;
    defaultApplications = associations;
    # tip use xdg-mime query filetype ./file
  };
}
