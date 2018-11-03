# Path to your oh-my-zsh installation.
export ZSH=/home/g/.oh-my-zsh
ZSH_THEME='pointer'
export UPDATE_ZSH_DAYS=13

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
alias bspConfig="nvim ~/.config/bspwm/bspwmrc"
alias sxConfig="nvim ~/.config/sxhkd/sxhkdrc"
alias i3Config="nvim ~/.config/i3/config"
alias termiteConfig="nvim ~/.config/termite/config"
alias nvimConfig="nvim ~/.config/nvim/init.vim"
alias comptonConfig="nvim ~/.config/compton.conf"
alias polybarConfig="nvim ~/.config/polybar/config"
alias promptConfig="nvim ~/.oh-my-zsh/custom/themes/pointer.zsh-theme"
alias rangerConfig="nvim ~/.config/ranger/rc.conf"
alias nf="neofetch"
alias stConfig="nvim ~/Projects/st-0.8.1/config.h"
# xcape -e 'Caps_Lock = Escape'
