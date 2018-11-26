#/usr/bin/bash
echo 'copying configuration files to ~/dotfiles...'
cp -r ~/.config/polybar ~/.config/i3 ~/.config/termite ~/.config/ranger ~/.config/compton.conf ~/.config/nvim ~/.config/bspwm ~/.config/sxhkd ~/.vimrc ~/.emacs ~/.zshrc ~/.i3blocks.conf ~/updateDotfiles.sh ~/scripts ~/dotfiles

cd ~/dotfiles && git status
echo 'push changes to github? [y/N]'
read resp
if [ $resp == 'y' ] || [ $resp == 'Y' ]; then
	git add .
	echo 'commit message:'
	read msg
	git commit -m "$msg"
	git push origin master
fi
echo 'Done'
