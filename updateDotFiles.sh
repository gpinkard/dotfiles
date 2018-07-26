!#/bin/bash

echo "Updating ~/dotfiles"
cp -r ~/.config/i3/ ~/dotfiles
cp -r ~/.config/termite/ ~/dotfiles
cp -r ~/.config/ranger/ ~/dotfiles
cp -r ~/.config/nvim/ ~/dotfiles
cp -r ~/.config/polybar/ ~/dotfiles
cp ~/.config/compton.conf ~/dotfiles
cp ~/.vimrc ~/dotfiles
cp ~/.zshrc ~/dotfiles
cp ~/.emacs ~/dotfiles
cp ~/updateDotFiles.sh ~/dotfiles
echo -e "Commit changes? [y/N]"
read com
if [ $com = "y" ] || [ $com = "Y" ]
then
	echo -e "enter commit message"
	read msg
	echo $msg
	cd ~/dotfiles
	git add .
	git commit -m "$msg"
	echo -e "Push changes? [y/N]"
	read push
	if [ $push = "y" ] || [ $push = "Y" ]
	then
		git push origin master
	fi
fi
