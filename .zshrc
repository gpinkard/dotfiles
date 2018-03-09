# Path to your oh-my-zsh installation.
export ZSH=/home/mfwic/.oh-my-zsh

# ZSH_THEME="powerlevel9k/powerlevel9k"
# PROMPT_GEOMETRY_COLORIZE_SYMBOL=true
GEOMETRY_PROMPT_PREFIX=""
GEOMETRY_GIT_SEPARATOR="--"
GEOMETRY_SYMBOL_PROMPT=" "
GEOMETRY_SYMBOL_EXIT_VALUE=""
GEOMETRY_COLOR_PROMPT="yellow"
GEOMETRY_COLOR_DIR="magenta"
GEOMETRY_COLOR_EXIT_VALUE="red"
ZSH_THEME='geometry/geometry'
# POWERLEVEL9K_MODE='awesome-fontconfig'
export UPDATE_ZSH_DAYS=13

COMPLETION_WAITING_DOTS="true"

# Plugings (add to ~/.oh-my-zsh/custom/plugins)
plugins=(
  git
	vi-mode
)

source $ZSH/oh-my-zsh.sh

# autocompletion
autoload -Uz compinit
compinit

export EDITOR=usr/bin/vim
export VISUAL=usr/bin/vim
# export TERM="xterm-256color"
alias termiteConfig="nvim ~/.config/termite/config"
alias nano="nvim"
alias nvimConfig="nvim ~/.config/nvim/init.vim"
alias comptonConfig="nvim ~/.config/compton.conf"
alias polybarConfig="nvim ~/.config/polybar/config"







