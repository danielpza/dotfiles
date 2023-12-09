# dotfiles

My (WIP) emacs and nixos home-manager configuration

Previous dotfiles of arch linux + dwm + emacs doom are available at https://github.com/danielpza/dotfiles-old

## Installation

```sh
git clone git@github.com:danielpza/dotfiles.git ~/.config/home-manager

nix run home-manager -- switch --flake path:/home/daniel/.config/home-manager#c1
```

## Reference

- https://nix-community.github.io/home-manager/index.xhtml#sec-install-standalone
- https://nix-community.github.io/home-manager/index.xhtml#_how_do_i_install_packages_from_nixpkgs_unstable
- on nixos see https://github.com/Mic92/nix-ld for fixing volta issues

## TODO

- [x] flakes (https://nixos.wiki/wiki/Flakes)
- [ ] DE agnostic setup (only apply dconf settings if gnome is installed, etc)
- [ ] use home-manager as nixos module (https://nixos.wiki/wiki/Home_Manager#Usage_as_a_NixOS_module)
