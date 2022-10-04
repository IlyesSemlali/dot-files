source $HOME/.zsh/utils.zsh

# PATH
append_path "/usr/local/lib64/ruby/gems/2.5.0/bin"
insert_path "$HOME/.local/bin"
insert_path "$HOME/.local/share/npm/bin"
insert_path "$HOME/.local/node_modules/.bin"
insert_path "$HOME/.krew/bin"


# HomeBrew
eval $($HOME/.local/share/homebrew/bin/brew shellenv)


# ZSH config
autoload -U compinit && compinit
zstyle ':urlglobber' url-other-schema


# Oh-My-ZSH Config
CASE_SENSITIVE="true"
ZSH_THEME="agnoster"
DISABLE_UPDATE_PROMPT=true
plugins=(git docker docker-compose)

export ZSH="$HOME/.oh-my-zsh"
source $ZSH/oh-my-zsh.sh


# Aliases and exports
alias ll="ls -ltrh"
alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox,.terraform} --exclude .zsh_history'
alias av="ansible-vault edit"
function pj() { source "$PROJECT_ROOT/.project" }

unset SSH_ASKPASS
export BAT_PAGER=''
export BAT_THEME='gruvbox-dark'

export SYSTEMD_PAGER=''
export VIRTUAL_ENV_DISABLE_PROMPT=1

export ENHANCD_DISABLE_DOT=1
export ENHANCD_DISABLE_HOME=1
export ENHANCD_HYPHEN_ARG='_'

# HomeBrew
eval $($HOME/.local/share/homebrew/bin/brew shellenv)


#NeoVIM
if which nvim >/dev/null 2>&1
then
    alias vim="nvim"
    export EDITOR=nvim
else
    export EDITOR=vim
fi

# Extensions

source $HOME/.zsh/dirstack.zsh
source $HOME/.zsh/prompt.zsh
source $HOME/.zsh/plugins.zsh

# Extra tools configuration
if which terraform > /dev/null 2>&1
then
    alias tf=terraform
    plugins+=(terraform)
    alias tfswitch="tfswitch -b ~/.local/bin/terraform"
fi

if which kubectl > /dev/null 2>&1
then
    alias k=kubectl
    source <(k completion zsh)
fi

if [ -f ~/.zshrc.local ]
then
    source ~/.zshrc.local
fi


