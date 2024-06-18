source ${HOME}/.zsh/utils.zsh

# HomeBrew
if [ -f ${HOME}/.local/share/homebrew/bin/brew ]
then
  eval $(${HOME}/.local/share/homebrew/bin/brew shellenv)
else
  echo "init-home: brew not installed, please run 'init-home -b'"
fi


# ZSH config
autoload -U compinit && compinit
zstyle ':urlglobber' url-other-schema
bindkey \^U backward-kill-line


# Oh-My-ZSH Config
CASE_SENSITIVE="true"
DISABLE_UPDATE_PROMPT=true
plugins=(git docker docker-compose fzf)

export ZSH="${HOME}/.oh-my-zsh"
if [ -f ${ZSH}/oh-my-zsh.sh ]
then
  source ${ZSH}/oh-my-zsh.sh
else
  echo "init-home: Oh-My-ZSH not installed, please run 'init-home -z'"
fi


# Aliases and exports
if which nvim >/dev/null 2>&1
then
  alias ls="exa --icons --level 1"
  alias l="exa -l --level 1 --git --no-permissions"
  alias ll="exa -al --level 1 --git-ignore --git"
else
  alias ls="ls --color=auto"
  alias l="ls --full-time -ltrh"
  alias ll="ls --full-time -altrh"
fi

eval "$(zoxide init --cmd cd zsh)"
eval "$(starship init zsh)"

alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox,.terraform} --exclude .zsh_history'
alias dv="cdi"
alias pj="sd project"
alias cpj="cd ${HOME}/projects/$PROJECT/"

unset SSH_ASKPASS

export SYSTEMD_PAGER=''
export VIRTUAL_ENV_DISABLE_PROMPT=1

export FZF_DEFAULT_OPTS="--height=25 --min-height=15 --border=rounded --margin=1 --padding=1 --border-label-pos=3"

export K9S_CONFIG_DIR="$HOME/.config/k9s/"

export _ZO_FZF_OPTS="--border-label='Change Directory' --preview 'exa -al --group-directories-first --no-user --no-time --no-filesize --no-permissions {2..}' --preview-window right,40% --height 40% --reverse --ansi"

export SD_ROOT="${HOME}/.local/sd"
export SD_CAT="bat"

export GOPATH="${HOME}/.local/share/go"

#NeoVIM
if which nvim >/dev/null 2>&1
then
  alias vim="nvim"
  export EDITOR=nvim
else
  export EDITOR=vim
fi

# Demo mode
if [ -z DEMO_MODE ]
then
  export HOST=laptop
fi

# Extensions
source ${HOME}/.zsh/plugins.zsh

# PATH
insert_path "${HOME}/.local/bin"
insert_path "${HOME}/.local/share/npm/bin"
insert_path "${HOME}/.local/node_modules/.bin"
insert_path "${HOME}/.krew/bin"


if [ -f ${HOME}/.zshrc.d/local.zsh ]
then
  source ${HOME}/.zshrc.d/local.zsh
fi

source ${HOME}/.zshrc.tools
source ${HOME}/.zshrc.d/project-management.zsh
