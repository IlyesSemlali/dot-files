source "${HOME}/.local/lib/log"
#
# # PowerLevel10k prompt
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

[[ ! -f ~/.zshrc.d/p10k.zsh ]] || source ~/.zshrc.d/p10k.zsh

source ${HOME}/.zsh/utils.zsh
source ${HOME}/.zshrc.d/plugin-manager.zsh
source ${HOME}/.zshrc.d/options.zsh

if [[ "$(uname)" == "Darwin" ]]; then
  source ${HOME}/.zshrc.d/homebrew.zsh
fi

###############################
##### Aliases and exports #####
###############################

if which eza >/dev/null 2>&1; then
  alias ls="eza --icons --level 1"
  alias l="eza --git-ignore --git --no-user --no-permissions --level 1 -l"
  alias ll="eza --git --no-user --no-permissions --level 1 -l"
  alias la="eza --git-ignore --git --level 1 -al"
else
  alias l='ls --color=auto -lh'
  alias ll='ls --color=auto -lah'
fi

alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox,.terraform} --exclude .zsh_history'
alias dv="cdi"

unset SSH_ASKPASS

# Send raw chars to terminal (render colors)
export LESS="-r"

export SYSTEMD_PAGER=''
export VIRTUAL_ENV_DISABLE_PROMPT=1

if [ -z "$TMUX" ]
then
  export FZF_DEFAULT_OPTS='--tmux=center,80%,60% --margin=1 --padding=1 --height=25 --min-height=15 --border=rounded --border-label-pos=3'
else
  export FZF_DEFAULT_OPTS='--tmux=center,80%,60% --margin=0 --padding=0'
fi

export K9S_CONFIG_DIR="$HOME/.config/k9s/"

export _ZO_FZF_OPTS="--border-label='Change Directory'
  --preview 'eza -al --group-directories-first --no-user --no-time --no-filesize --no-permissions {2..}'
  --bind 'ctrl-a:reload(zoxide edit increment {2..})'
  --bind 'ctrl-x:reload(zoxide edit decrement {2..})'
  --preview-window right,40% --height 40% --reverse --ansi"

export SD_ROOT="${HOME}/.local/sd"
export SD_CAT="bat"

export GOPATH="${HOME}/.local/share/go"

export PRE_COMMIT_ALLOW_NO_CONFIG=1

###############################

#####################################
##### Shell customization tools #####
#####################################

# Fuzzy-find CD replacement
(( $+commands[zoxide])) && eval "$(zoxide init --cmd cd zsh)"

# Themes for ls and tree using sharkdp/vivid
if (( $+commands[vivid])); then
  if [ -f ${HOME}/.themes/current/vivid.yaml ]; then
    export LS_COLORS="$(vivid generate ${HOME}/.themes/current/vivid.yaml)"
  else
    export LS_COLORS="$(vivid generate ${HOME}/.themes/gruvbox-dark/vivid.yaml)"
  fi
fi

#####################################

#NeoVIM
if which nvim >/dev/null 2>&1; then
  alias vim="nvim"
  export EDITOR=nvim
else
  export EDITOR=vim
fi

# PATH
insert_path "${HOME}/.local/bin"
insert_path "${HOME}/.local/share/npm/bin"
insert_path "${HOME}/.local/node_modules/.bin"
insert_path "${HOME}/.krew/bin"
append_path "${HOME}/.local/share/nvim/mason/bin"

# GoLang
if [ -e ${GOPATH}/bin ]; then
  append_path "$GOPATH/bin"
fi

if [ -f ${HOME}/.zshrc.d/local.zsh ]; then
  source ${HOME}/.zshrc.d/local.zsh
fi

source ${HOME}/.zshrc.d/completion.zsh
source ${HOME}/.zshrc.d/tools.zsh
source ${HOME}/.zshrc.d/keybindings.zsh
source ${HOME}/.zshrc.d/project-management.zsh
