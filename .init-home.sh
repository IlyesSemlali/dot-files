#!/usr/bin/env zsh

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

function install_tpm() {

    source ~/.zshrc

    # tmux TPM
    install_from_git https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    ${HOME}/.tmux/plugins/tpm/bin/clean_plugins
    ${HOME}/.tmux/plugins/tpm/bin/install_plugins
}

install_brew
install_tpm
