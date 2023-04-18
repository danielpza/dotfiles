# dotfiles

My (WIP) emacs and nixos home-manager configuration:

## Installation

- Clone config

```sh
git clone git@github.com:danielpza/dotfiles.git ~/.config/home-manager
```

- Follow home-manager installation instructions at https://nix-community.github.io/home-manager/index.html#ch-installation, typically involves:

```sh
nix-shell '<home-manager>' -A install
```

- Then run

```sh
home-manager switch
```
