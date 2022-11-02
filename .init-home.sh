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

    gnupaths="${HOME}/.cache/gnupaths"
    rm ${gnupaths}

    source ~/.zshrc

    # tmux TPM
    install_from_git https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    ${HOME}/.tmux/plugins/tpm/bin/clean_plugins
    ${HOME}/.tmux/plugins/tpm/bin/install_plugins
}

install_tpm
install_brew
