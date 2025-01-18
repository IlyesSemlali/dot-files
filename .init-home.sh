#!/usr/bin/env zsh

source "${HOME}/.local/lib/log"

# HomeBrew
if [ -f $HOME/.local/share/homebrew/bin/brew ]; then
    eval $($HOME/.local/share/homebrew/bin/brew shellenv)
fi


PATH="$PATH:~/.local/bin/"

function install_from_git() {
    git_bin=$(which git)
    repo=${1}
    install_path=${2}

    if [ -d ${install_path} ]; then
        git -C ${install_path} pull
    else
        git clone ${repo} ${install_path}
    fi
    unset install_path
    unset repo
}

function install_brew() {
    # Brew for mac
    install_from_git https://github.com/Homebrew/brew ~/.local/share/homebrew

    source ~/.zshrc

    brew install rcmdnk/file/brew-file || {
        log_error "init" "openssl probably failed to install, run:
        brew install --debug --verbose openssl@1.1
        and press continue when prompted"
    }

    brew file install
}

function install_tpm() {

    source ~/.zshrc

    # tmux TPM
    install_from_git https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    ${HOME}/.tmux/plugins/tpm/bin/clean_plugins
    ${HOME}/.tmux/plugins/tpm/bin/install_plugins
}

function _get_config_keys() {
    grep '=' $1 | awk '{ print $1 }' | grep -v '^[\s-]*$'
}

function configure_xmonad() {
    XMONAD_CONFIG_TEMPLATE="/home/$USER/.xmonad/lib/Config.hs.tpl"
    XMONAD_CONFIG="/home/$USER/.xmonad/lib/Config.hs"

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

install_tpm
install_brew
configure_xmonad
