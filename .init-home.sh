#!/bin/bash

XMONAD_CONFIG_TEMPLATE="/home/$USER/.xmonad/lib/Config.hs.tpl"
XMONAD_CONFIG="/home/$USER/.xmonad/lib/Config.hs"

TFLS_VERSION="0.27.0"

OS=$(get_platform | cut -d '-' -f1)

function install_vim_plugins() {
    echo "-- Installing VIM Plugins --"
    /usr/bin/vim -N -u ~/.viminitrc
    /usr/bin/vim -N -u ~/.vim-install-plugins
    which nvim 2>&1 > /dev/null \
        && nvim -N -u ~/.viminitrc \
        && nvim -N -u ~/.vim-install-plugins
}

function reset_omz() {
    echo "-- Installing OhMyZSH --"

    rm -rf .oh-my-zsh/
    mkdir -p ~/.cache
    touch ~/.cache/zshdirs
    sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh) --keep-zshrc --skip-chsh --unattended > /dev/null 2>&1"
    git clone https://github.com/macunha1/zsh-terraform ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/terraform
    git -C ~ reset --hard > /dev/null 2>&1
}

function _get_config_keys() {
    grep '=' $1 | awk '{ print $1 }' | grep -v '^[\s-]*$'
}

function configure_dolphin() {
    if [[ $OS == "linux" ]]; then
        if [ ! -f ~/.config/dolphinrc ]
        then
            cp ~/.config/dolphinrc.tpl ~/.config/dolphinrc
        fi
    fi
}

function configure_xmonad() {
    if [[ $OS == "linux" ]]; then
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
    fi
}

function install_language_servers() {
    pip3 install --user ansible-lint

    cd ~/.local/
    yarn add ansible-language-server || npm i ansible-language-server
    yarn add yaml-language-server || npm i yaml-language-server
    yarn add bash-language-server || npm i bash-language-server
    cd -

    cd ~/.local/bin
    if [[ $OS == mac ]]; then
        tfls_release=darwin_arm64
    else
        tfls_release=linux_amd64
    fi
    wget https://releases.hashicorp.com/terraform-ls/${TFLS_VERSION}/terraform-ls_${TFLS_VERSION}_${tfls_release}.zip
    unzip -o terraform-ls_${TFLS_VERSION}_${tfls_release}.zip
    rm terraform-ls_${TFLS_VERSION}_${tfls_release}.zip
    cd -
}

function install_tmux_powerline () {
    if ! [ -d ~/.local/share/tmux-powerline ]; then
        git clone https://github.com/erikw/tmux-powerline ~/.local/share/tmux-powerline
    fi
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


if [[ $RESET_OMZ == 'true' ]]
then
    install_tmux_powerline
    reset_omz
fi
