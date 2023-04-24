# dotfiles

My (WIP) emacs and nixos home-manager configuration:

## Installation

- Clone config

```sh
git clone git@github.com:danielpza/dotfiles.git ~/.config/home-manager
```

- Follow home-manager installation instructions at https://nix-community.github.io/home-manager/index.html#ch-installation, typically involves:

```sh
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
nix-channel --update

nix-shell '<home-manager>' -A install
```

- Then run

```sh
home-manager switch
```

- If installing on non-nixos and not managing your shell configuration through home-manager add these lines to your .bashrc file:

```sh
. "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
```

## Reference

- https://nix-community.github.io/home-manager/index.html#sec-install-standalone
- https://nix-community.github.io/home-manager/index.html#_how_do_i_install_packages_from_nixpkgs_unstable
