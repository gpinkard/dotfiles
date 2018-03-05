# Path to your oh-my-zsh installation.
export ZSH=/home/mfwic/.oh-my-zsh

ZSH_THEME="powerlevel9k/powerlevel9k"
POWERLEVEL9K_MODE='awesome-fontconfig'
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

alias i3Config="nvim ~/.config/i3/config"
alias termiteConfig="nvim ~/.config/termite/config"
alias nano="nvim"
alias nvimConfig="nvim ~/.config/nvim/init.vim"
alias comptonConfig="nvim ~/.config/compton.conf"
alias polybarConfig="nvim ~/.config/polybar/config"

POWERLEVEL9K_HOME_ICON=''
POWERLEVEL9K_FOLDER_ICON='  '
POWERLEVEL9K_ROOT_ICON=' '
POWERLEVEL9K_USER_ICON=' '
POWERLEVEL9K_HOST_ICON=' '
POWERLEVEL9K_SHORTEN_DIR_LENGTH=3
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
# POWERLEVEL9K_PROMPT_ADD_NEWLINE=true
POWERLEVEL9K_SHORTEN_DELIMITER='...'
POWERLEVEL9K_PROMPT_ALWAYS_SHOW_CONTEXT=true
POWERLEVEL9K_TIME_FORMAT=' %D{%H:%M:%S}'
POWERLEVEL9K_DIR_SHOW_WRITABLE='true'
POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=""
POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="   "
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(os_icon user dir vcs) 
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(ssh root_indicator vi_mode host time)
