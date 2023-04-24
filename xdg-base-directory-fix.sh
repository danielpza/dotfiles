#!/usr/bin/env sh

# https://stackoverflow.com/a/28085062/6051261
# https://wiki.archlinux.org/title/XDG_Base_Directory#Support
: "${XDG_CONFIG_HOME:=$HOME/.config}"
: "${XDG_CACHE_HOME:=$HOME/.cache}"
: "${XDG_DATA_HOME:=$HOME/.local/share}"
: "${XDG_STATE_HOME:=$HOME/.local/state}"

export XDG_CONFIG_HOME
export XDG_CACHE_HOME
export XDG_DATA_HOME
export XDG_STATE_HOME

includedir() {
	[[ -d $1 ]] && PATH="$1:$PATH"
}

export PYENV_ROOT="$XDG_DATA_HOME/pyenv" # pyenv
includedir "$PYENV_ROOT/bin"             # pyenv
export VOLTA_HOME="$XDG_DATA_HOME/volta" # volta
includedir "$VOLTA_HOME/bin"             # volta
export BUN_INSTALL="$HOME/.bun"          # bun
includedir "$BUN_INSTALL/bin"            # bun
export CARGO_HOME="$XDG_DATA_HOME/cargo" # rust cargo
export SSB_HOME="$XDG_DATA_HOME/zoom"    # zoom
export GOPATH="$XDG_DATA_HOME/go"        # go
includedir "$GOPATH/bin"                 # go

export PNPM_HOME="/home/daniel/.local/share/pnpm"
includedir "$PNPM_HOME/bin"

export WGETRC="$XDG_CONFIG_HOME/wgetrc"
touch "$XDG_CONFIG_HOME/wgetrc"
alias wget=wget --hsts-file="$XDG_CACHE_HOME/wget-hsts"

# npm https://github.com/npm/rfcs/issues/389#issuecomment-871656832
export npm_config_userconfig=$XDG_CONFIG_HOME/npm/config
export npm_config_cache=$XDG_CACHE_HOME/npm
export npm_config_prefix=$XDG_DATA_HOME/npm
includedir $XDG_DATA_HOME/npm/bin

export NODE_REPL_HISTORY="$XDG_DATA_HOME/node_repl_history"

# gtk
export GTK_RC_FILES="$XDG_CONFIG_HOME"/gtk-1.0/gtkrc
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc

# kde
export KDEHOME="$XDG_CONFIG_HOME"/kde
