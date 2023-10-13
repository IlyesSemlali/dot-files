source $HOME/.zsh/utils.zsh

# PATH
append_path "/usr/local/lib64/ruby/gems/2.5.0/bin"
append_path "$HOME/.krew/bin"
insert_path "$HOME/.local/bin"
insert_path "$HOME/.local/share/npm/bin"
insert_path "$HOME/.local/node_modules/.bin"
insert_path "$HOME/.krew/bin"


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
plugins=(git docker docker-compose)

export ZSH="$HOME/.oh-my-zsh"
if [ -f $ZSH/oh-my-zsh.sh ]; then
    source $ZSH/oh-my-zsh.sh
else
    echo "init-home: Oh-My-ZSH not installed, please run 'init-home -z'"
fi


# Aliases and exports
alias ls="ls --color=auto --full-time"
alias ll="ls -ltrh"
alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox,.terraform} --exclude .zsh_history'
alias av="ansible-vault edit"
alias dv="cd _"
alias pj="project-switcher"

unset SSH_ASKPASS
export BAT_PAGER=''
export BAT_THEME='gruvbox-dark'

export SYSTEMD_PAGER=''
export VIRTUAL_ENV_DISABLE_PROMPT=1

export ENHANCD_DISABLE_DOT=1
export ENHANCD_DISABLE_HOME=1
export ENHANCD_HYPHEN_ARG='_'
export ENHANCD_HYPHEN_NUM=20
export ENHANCD_FILTER="fzf --border-label='Change Directory'"

export FZF_DEFAULT_OPTS="--height=25 --min-height=15 --border=rounded --margin=1 --padding=1 --border-label-pos=3"

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


if [ -f ~/.zshrc.tools ]
then
    source ~/.zshrc.tools
fi

if [ -f ~/.zshrc.local ]
then
    source ~/.zshrc.local
fi


