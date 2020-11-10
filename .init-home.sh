#!/bin/bash

XMONAD_CONFIG_TEMPLATE="/home/$USER/.xmonad/lib/Config.hs.tpl"
XMONAD_CONFIG="/home/$USER/.xmonad/lib/Config.hs"


function install_vim_plugins() {
	git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
	vim +PluginInstall +qall
}

function reset_omz() {
	echo "-- Installing OhMyZSH --"

	rm -rf .oh-my-zsh/
	sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
	git -C ~ reset --hard
}

function _get_config_keys() {
	grep '=' $1 | awk '{ print $1 }' | grep -v '^[\s-]*$'
}

function configure_xmonad() {
	if [ ! -f $XMONAD_CONFIG ]
	then
		echo "-- Adding a fresh Xmonad config --"
		sed "s/user/$USER/g" $XMONAD_CONFIG_TEMPLATE > $XMONAD_CONFIG
	fi

	for config_key in $(_get_config_keys $XMONAD_CONFIG_TEMPLATE)
	do
		if ! grep -q "^$config_key" $XMONAD_CONFIG
		then
			echo "-- Adding $config_key in Xmonad configuration --"
			#grep "^$config_key" $XMONAD_CONFIG_TEMPLATE >> $XMONAD_CONFIG
		fi
	done

	cd ~/.xmonad/lib/ && ghc --make Config.hs; cd
}

reset_omz
configure_xmonad
