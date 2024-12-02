#!/usr/bin/env bash

source "${HOME}/.local/lib/log"

PATH="$PATH:~/.local/bin/"

function install_apt() {
    # Install from APT
    sudo apt update
    sudo apt install nvim bat zsh exa zoxide fzf node
}

install_apt
