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

