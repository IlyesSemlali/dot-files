#!/usr/bin/env zsh

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
    # Install from APT
    sudo apt update
    sudo apt install nvim bat zsh exa zoxide fzf node
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
