!#/bin/bash

echo "Updating ~/dotfiles"
cp -r ~/.config/i3/ ~/dotfiles
cp -r ~/.config/termite/ ~/dotfiles
cp -r ~/.config/ranger/ ~/dotfiles
cp -r ~/.config/nvim/ ~/dotfiles
cp -r ~/.config/polybar/ ~/dotfiles
cp ~/.config/compton.conf ~/dotfiles
cp .vimrc ~/dotfiles
cp .zshrc ~/dotfiles
cp .emacs ~/dotfiles
echo -e "Commit and push to github? [y/N]"
read resp
if [ $resp = "y" ] || [ $resp = "Y" ]
then
	echo -e "enter commit message"
	read msg
	cd ~/dotfiles
	git add .
	git commit -m $msg
	git push origin master
fi
