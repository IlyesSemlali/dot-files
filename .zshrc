source $HOME/.zsh/utils.zsh

# HomeBrew
if [ -f $HOME/.local/share/homebrew/bin/brew ]; then
    eval $($HOME/.local/share/homebrew/bin/brew shellenv)
else
    echo "init-home: brew not installed, please run 'init-home -b'"
fi


# ZSH config
autoload -U compinit && compinit
zstyle ':urlglobber' url-other-schema


# Oh-My-ZSH Config
CASE_SENSITIVE="true"
ZSH_THEME="agnoster"
DISABLE_UPDATE_PROMPT=true
plugins=(git docker docker-compose fzf)

export ZSH="$HOME/.oh-my-zsh"
if [ -f $ZSH/oh-my-zsh.sh ]; then
    source $ZSH/oh-my-zsh.sh
else
    echo "init-home: Oh-My-ZSH not installed, please run 'init-home -z'"
fi


# Aliases and exports
if which nvim >/dev/null 2>&1
then
  alias ls="exa --icons --level 1 --git-ignore --git"
  alias l="exa -l --level 1 --git --no-permissions"
  alias ll="exa -al --level 1 --git-ignore --git --no-permissions"
else
  alias ls="ls --color=auto"
  alias l="ls --full-time -ltrh"
  alias ll="ls --full-time -altrh"
fi

alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox,.terraform} --exclude .zsh_history'
alias av="ansible-vault edit"
alias dv="__enhancd::cd _"
alias pj="sd project"
alias cpj="cd ~/projects/$PROJECT/"

unset SSH_ASKPASS

export SYSTEMD_PAGER=''
export VIRTUAL_ENV_DISABLE_PROMPT=1

export ENHANCD_ENABLE_DOT="false"
export ENHANCD_ENABLE_DOUBLE_DOT="false"
export ENHANCD_ENABLE_HOME="false"
export ENHANCD_ARG_HYPHEN='_'
export ENHANCD_HYPHEN_NUM=20
export ENHANCD_FILTER="fzf --border-label='Change Directory' --preview 'exa -al --level 1 --group-directories-first --git-ignore --git --no-user --no-time --no-filesize --no-permissions {}' --preview-window right,50% --height 35% --reverse --ansi"

export FZF_DEFAULT_OPTS="--height=25 --min-height=15 --border=rounded --margin=1 --padding=1 --border-label-pos=3"

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
if [ -z DEMO_MODE ]; then
    export HOST=laptop
fi

# Extensions
source $HOME/.zsh/prompt.zsh
source $HOME/.zsh/plugins.zsh

# PATH
insert_path "$HOME/.local/bin"
insert_path "$HOME/.local/share/npm/bin"
insert_path "$HOME/.local/node_modules/.bin"
insert_path "$HOME/.krew/bin"


unalias cd # For enhancd

if [ -f ~/.zshrc.tools ]
then
    source ~/.zshrc.tools
fi

if [ -f ~/.zshrc.local ]
then
    source ~/.zshrc.local
fi

if [ -f ~/project/$PROJECT/.project ]
then
    source ~/project/$PROJECT/.project
fi

