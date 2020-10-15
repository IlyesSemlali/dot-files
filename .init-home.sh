#!/bin/bash

git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

rm -rf .oh-my-zsh/
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
git reset --hard

sed "s/ilyes/$USER/g" -i ~/.xmonad/lib/Config.hs
cd ~/.xmonad/lib/ && ghc --make Config.hs; cd


sed "s/ilyes/$USER/g" -i ~/.config/nitrogen/bg-saved.cfg ~/.config/nitrogen/nitrogen.cfg 
