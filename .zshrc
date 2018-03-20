# Path to your oh-my-zsh installation.
export ZSH=/home/gabe/.oh-my-zsh

# PROMPT STUFF
ZSH_THEME='gabe'
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

export EDITOR=/usr/bin/nvim
export VISUAL=/usr/bin/nvim
# export TERM="xterm-256color"
alias i3Config="nvim ~/.config/i3/config"
alias termiteConfig="nvim ~/.config/termite/config"
alias nvimConfig="nvim ~/.config/nvim/init.vim"
alias comptonConfig="nvim ~/.config/compton.conf"
alias polybarConfig="nvim ~/.config/polybar/config"
alias promptConfig="nvim ~/.oh-my-zsh/themes/gabe"
alias nf="neofetch"
