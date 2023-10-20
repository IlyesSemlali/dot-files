#!/usr/bin/env zsh

# HomeBrew
if [ -f $HOME/.local/share/homebrew/bin/brew ]; then
    eval $($HOME/.local/share/homebrew/bin/brew shellenv)
fi


XMONAD_CONFIG_TEMPLATE="/home/$USER/.xmonad/lib/Config.hs.tpl"
XMONAD_CONFIG="/home/$USER/.xmonad/lib/Config.hs"

TFLS_VERSION="0.28.1"

OS=$(~/.local/bin/get_platform | cut -d '-' -f1)

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

function install_nvchad() {
    git_bin=$(which git)
    repo="https://github.com/NvChad/NvChad"
    install_path="${HOME}/.config/nvim"

    if [ -f ${install_path}/LICENSE ]; then
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
        echo
        echo
        echo "openssl probably failed to install, run:"
        echo "brew install --debug --verbose openssl@1.1"
        echo
        echo "and press continue when prompted"
        exit 1
    }

    brew file install
}

function install_tools() {

    source ~/.zshrc

    # ZPlug
    install_from_git https://github.com/zplug/zplug ~/.local/share/zplug

    # tmux TPM
    install_from_git https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

    # tmux powerline
    install_from_git https://github.com/IlyesSemlali/tmux-powerline ~/.local/share/tmux-powerline

    # tfswitch
    if ! which tfswitch > /dev/null 2>&1; then
        curl -L https://raw.githubusercontent.com/warrensbox/terraform-switcher/release/install.sh | sudo bash
    fi

    # Krew
    (
        cd "$(mktemp -d)" &&
        OS="$(uname | tr '[:upper:]' '[:lower:]')" &&
        ARCH="$(uname -m | sed -e 's/x86_64/amd64/' -e 's/\(arm\)\(64\)\?.*/\1\2/' -e 's/aarch64$/arm64/')" &&
        KREW="krew-${OS}_${ARCH}" &&
        curl -fsSLO "https://github.com/kubernetes-sigs/krew/releases/latest/download/${KREW}.tar.gz" &&
        tar zxvf "${KREW}.tar.gz" &&
        ./"${KREW}" install krew
    )
}

function configure_zsh() {
    echo "-- Installing OhMyZSH --"

    rm -rf .oh-my-zsh/
    mkdir -p ~/.cache
    touch ~/.cache/zshdirs
    sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh) --keep-zshrc --skip-chsh --unattended > /dev/null 2>&1"
    install_from_git https://github.com/macunha1/zsh-terraform ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/terraform
    git -C ~ reset --hard > /dev/null 2>&1

    source ~/.zshrc

    if ! zplug check --verbose; then
        zplug install
    fi
}

function _get_config_keys() {
    grep '=' $1 | awk '{ print $1 }' | grep -v '^[\s-]*$'
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

while [[ $# -gt 0 ]]
do
    key="$1"

    case $key in
        -b|--brew)
            BREW='true'
            shift
            ;;
        -v|--vim)
            VIM='true'
            shift
            ;;
        -x|--xmonad)
            XMONAD='true'
            shift
            ;;
        -t|--tools)
            TOOLS='true'
            shift
            ;;
        -z|--zsh)
            CONFIGURE_ZSH='true'
            shift
            ;;
    esac
done

if [[ $BREW == 'true' ]]
then
    install_brew
fi


if [[ $TOOLS == 'true' ]]
then
    install_tools
fi


if [[ $VIM == 'true' ]]
then
    install_nvchad
fi


if [[ $XMONAD == 'true' ]]
then
    configure_xmonad
fi


if [[ $CONFIGURE_ZSH == 'true' ]]
then
    configure_zsh
fi
