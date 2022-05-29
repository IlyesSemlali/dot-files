#!/bin/bash

XMONAD_CONFIG_TEMPLATE="/home/$USER/.xmonad/lib/Config.hs.tpl"
XMONAD_CONFIG="/home/$USER/.xmonad/lib/Config.hs"

OBSIDIAN_VERSION="0.12.19"

function install_vim_plugins() {
    echo "-- Installing VIM Plugins --"
    /usr/bin/vim -N -u ~/.viminitrc
    /usr/bin/vim -N -u ~/.vim-install-plugins
    which nvim 2>&1 > /dev/null \
        && nvim -N -u ~/.viminitrc \
        && nvim -N -u ~/.vim-install-plugins
}

function install_obsidian() {
    wget https://github.com/obsidianmd/obsidian-releases/releases/download/v$OBSIDIAN_VERSION/Obsidian-$OBSIDIAN_VERSION.AppImage -O ~/.local/bin/Obsidian-$OBSIDIAN_VERSION.AppImage
    ln -sf ~/.local/bin/{Obsidian-0.12.19.AppImage,obsidian}
    chmod +x ~/.local/bin/Obsidian-$OBSIDIAN_VERSION.AppImage
}

function reset_omz() {
    echo "-- Installing OhMyZSH --"

    rm -rf .oh-my-zsh/
    mkdir ~/.cache -p
    touch ~/.cache/zshdirs
    sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh) --keep-zshrc --skip-chsh --unattended > /dev/null 2>&1"
    git clone https://github.com/macunha1/zsh-terraform ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/terraform
    git -C ~ reset --hard > /dev/null 2>&1
}

function _get_config_keys() {
    grep '=' $1 | awk '{ print $1 }' | grep -v '^[\s-]*$'
}

function configure_dolphin() {
    if [ ! -f ~/.config/dolphinrc ]
    then
        cp ~/.config/dolphinrc.tpl ~/.config/dolphinrc
    fi
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
            grep "^$config_key" $XMONAD_CONFIG_TEMPLATE >> $XMONAD_CONFIG
        fi
    done

    cd ~/.xmonad/lib/
    which ghc 2>&1 > /dev/null && ghc --make Config.hs
    cd
}

function install_language_servers() {
    pip3 install --user ansible-lint

    cd ~/.local/
    yarn add ansible-language-server || npm i ansible-language-server
    yarn add yaml-language-server || npm i yaml-language-server
    yarn add bash-language-server || npm i bash-language-server
    cd -

    cd ~/.local/bin
    wget https://releases.hashicorp.com/terraform-ls/0.25.0/terraform-ls_0.25.0_linux_amd64.zip
    unzip -o terraform-ls_0.25.0_linux_amd64.zip
    rm terraform-ls_0.25.0_linux_amd64.zip
    cd -
}

while [[ $# -gt 0 ]]
do
    key="$1"

    case $key in
        -v|--vim)
            VIM='true'
            shift
            ;;
        -x|--xmonad)
            XMONAD='true'
            shift
            ;;
        -o|--obsidian)
            OBSIDIAN='true'
            shift
            ;;
        -z|--zsh)
            RESET_OMZ='true'
            shift
            ;;
    esac
done

configure_dolphin


if [[ $VIM == 'true' ]]
then
    install_vim_plugins
    install_language_servers
fi


if [[ $XMONAD == 'true' ]]
then
    configure_xmonad
fi


if [[ $OBSIDIAN == 'true' ]]
then
    install_obsidian
fi


if [[ $RESET_OMZ == 'true' ]]
then
    reset_omz
fi
