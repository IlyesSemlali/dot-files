#!/usr/bin/env bash

source "${HOME}/.local/lib/log"

# HomeBrew
if [[ "$(uname)" == "Darwin" ]]; then
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

  brew install rcmdnk/file/brew-file
  brew file install
}

function install_apt() {
    sudo apt update
    sudo apt install -y bat fzf zsh gpg nodejs starship
    sudo snap install --beta nvim --classic

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

function install_tpm() {

  source ~/.zshrc

  # tmux TPM
  install_from_git https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
  ${HOME}/.tmux/plugins/tpm/bin/clean_plugins
  ${HOME}/.tmux/plugins/tpm/bin/install_plugins
}

install_apt
install_tpm
if [[ "$(uname)" == "Darwin" ]]; then
  install_brew
fi
mise install
