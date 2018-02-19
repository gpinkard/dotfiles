# Path to your oh-my-zsh installation.
export ZSH=/home/mfwic/.oh-my-zsh

ZSH_THEME="powerlevel9k/powerlevel9k"
#ZSH_THEME="lambda"

export UPDATE_ZSH_DAYS=13

COMPLETION_WAITING_DOTS="true"

# Plugings (add to ~/.oh-my-zsh/custom/plugins)
plugins=(
  git
)

source $ZSH/oh-my-zsh.sh

# autocompletion
autoload -Uz compinit
compinit

export EDITOR=usr/bin/vim
export VISUAL=usr/bin/vim
export TERM="xterm-256color"

alias i3Config="nvim ~/.config/i3/config"
alias termiteConfig="nvim ~/.config/termite/config"
alias nano="nvim"
alias nvimConfig="nvim ~/.config/nvim/init.vim"
alias comptonConfig="nvim ~/.config/compton.conf"
alias polybarConfig="nvim ~/.config/polybar/config"
#POWERLEVEL9K_HOME_ICON=""

