# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
ZSH_THEME='pointer'
export UPDATE_ZSH_DAYS=13

# Plugings (add to ~/.oh-my-zsh/custom/plugins)
plugins=(
	git
)

source $ZSH/oh-my-zsh.sh

# autocompletion
autoload -Uz compinit
compinit

# export TERM="xterm-256color"
alias nf="neofetch"

# acme related stuff, build yourself, don't use packaged version
# https://github.com/9fans/plan9port
export PLAN9="$HOME/9/plan9port"
export PATH="$PATH:$PLAN9/bin"
export ACME_DIR="$PLAN9/src/cmd/acme"
alias acme="$ACME_DIR/bin/acme_start"
