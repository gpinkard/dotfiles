# Path to your oh-my-zsh installation.
export ZSH=/home/g/.oh-my-zsh
# ZSH_THEME='pointer'
POWERLEVEL9K_MODE='awesome-fontconfig'
ZSH_THEME='powerlevel9k/powerlevel9k'
export UPDATE_ZSH_DAYS=13
# Current dir info
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(os_icon dir dir_writable vcs)
# user info + other stuff
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(context background_jobs vi_mode status)
POWERLEVEL9K_FOLDER_ICON=''
POWERLEVEL9K_HOME_SUB_ICON=''
POWERLEVEL9K_HOME_ICON=''
POWERLEVEL9K_SHORTEN_DIR_LENGTH=2
POWERLEVEL9K_VI_INSERT_MODE_STRING="INS"
POWERLEVEL9K_VI_COMMAND_MODE_STRING="NOR"
# POWERLEVEL9K_VI_NORMAL_MODE_STRING="NOR"
# POWERLEVEL9K_CONTEXT_BACKGROUND="021"
POWERLEVEL9K_VI_MODE_INSERT_BACKGROUND="blue"
POWERLEVEL9K_VI_MODE_INSERT_FOREGROUND="black"
POWERLEVEL9K_VI_MODE_NORMAL_BACKGROUND="black"
POWERLEVEL9K_VI_MODE_NORMAL_FOREGROUND="blue"
POWERLEVEL9K_CONTEXT_DEFAULT_BACKGROUND="yellow"
POWERLEVEL9K_CONTEXT_DEFAULT_FOREGROUND="black"
POWERLEVEL9K_CONTEXT_ROOT_BACKGROUND="red"
POWERLEVEL9K_CONTEXT_ROOT_FOREGROUND="black"
# Plugings (add to ~/.oh-my-zsh/custom/plugins)
plugins=(
	git
	vi-mode
)

source $ZSH/oh-my-zsh.sh

# autocompletion
autoload -Uz compinit
compinit

# export TERM="xterm-256color"
alias i3Config="nvim ~/.config/i3/config"
alias termiteConfig="nvim ~/.config/termite/config"
alias nvimConfig="nvim ~/.config/nvim/init.vim"
alias comptonConfig="nvim ~/.config/compton.conf"
alias polybarConfig="nvim ~/.config/polybar/config"
alias promptConfig="nvim ~/.oh-my-zsh/custom/themes/pointer.zsh-theme"
alias rangerConfig="nvim ~/.config/ranger/rc.conf"
alias nf="neofetch"
# xcape -e 'Caps_Lock = Escape'
