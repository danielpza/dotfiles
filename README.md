# dotfiles

My (WIP) emacs and nixos home-manager configuration

Previous dotfiles of arch linux + dwm + emacs doom are available at https://github.com/danielpza/dotfiles-old

## Installation

```sh
git clone git@github.com:danielpza/dotfiles.git ~/.config/home-manager

cp /etc/nixos/hardware-configuration.nix .

nix run home-manager -- switch --flake path:/home/daniel/.config/home-manager#c1

sudo nixos-rebuild switch --flake path:/home/daniel/.config/home-manager#c1
```

## Reference

- https://nix-community.github.io/home-manager/index.xhtml#sec-install-standalone
- https://nix-community.github.io/home-manager/index.xhtml#_how_do_i_install_packages_from_nixpkgs_unstable
- on nixos see https://github.com/Mic92/nix-ld
