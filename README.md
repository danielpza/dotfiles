# dotfiles

My (WIP) emacs and nixos home-manager configuration

Previous dotfiles of arch linux + dwm + emacs doom are available at https://github.com/danielpza/dotfiles-old

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

Then choose what configuration to use:

```sh
cd .config/home-manager
ln -sfr $PWD/<c1|c2>.nix home.nix
```

- Then run

```sh
home-manager switch
```

## Reference

- https://nix-community.github.io/home-manager/index.html#sec-install-standalone
- https://nix-community.github.io/home-manager/index.html#_how_do_i_install_packages_from_nixpkgs_unstable

## TODO

- [ ] flakes (https://nixos.wiki/wiki/Flakes)
- [ ] DE agnostic setup (only apply dconf settings if gnome is installed, etc)
