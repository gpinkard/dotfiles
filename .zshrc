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

alias i3Config="vim ~/.config/i3/config"
alias termiteConfig="vim ~/.config/termite/config"
alias nano="vim"
alias comptonConfig="vim ~/.config/compton.conf"
POWERLEVEL9K_HOME_ICON=""

