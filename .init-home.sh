#!/usr/bin/env bash

source "${HOME}/.local/lib/log"

PATH="$PATH:~/.local/bin/"

function install_apt() {
    sudo apt update
    sudo apt install -y bat fzf zsh gpg neovim nodejs starship

    # Install zoxide
    curl -sSfL https://raw.githubusercontent.com/ajeetdsouza/zoxide/main/install.sh | sh

    # Install starship
    curl -sS https://starship.rs/install.sh -o install-starhip.sh
    sudo sh ./install-starhip.sh -y
    rm -f install_name_tool.sh

    # Install eza
    sudo mkdir -p /etc/apt/keyrings
    wget -qO- https://raw.githubusercontent.com/eza-community/eza/main/deb.asc | sudo gpg --dearmor -o /etc/apt/keyrings/gierens.gpg
    echo "deb [signed-by=/etc/apt/keyrings/gierens.gpg] http://deb.gierens.de stable main" | sudo tee /etc/apt/sources.list.d/gierens.list
    sudo chmod 644 /etc/apt/keyrings/gierens.gpg /etc/apt/sources.list.d/gierens.list
    sudo apt update
    sudo apt install -y eza
}

install_apt
