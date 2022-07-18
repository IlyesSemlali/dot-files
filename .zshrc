source $HOME/.zsh/utils.zsh

# PATH
append_path "$HOME/.local/bin"
append_path "/usr/local/lib64/ruby/gems/2.5.0/bin"
insert_path "$HOME/.local/share/npm/bin"
insert_path "$HOME/.local/node_modules/.bin"
insert_path "$HOME/.krew/bin"


# ZSH config
autoload -U compinit && compinit
zstyle ':urlglobber' url-other-schema


# Oh-My-ZSH Config
CASE_SENSITIVE="true"
ZSH_THEME="agnoster"
DISABLE_UPDATE_PROMPT=true
plugins=(git docker docker-compose)

if which terraform > /dev/null 2>&1
then
    plugins+=(terraform)
fi

export ZSH="$HOME/.oh-my-zsh"
source $ZSH/oh-my-zsh.sh


# Aliases and exports
alias ll="ls -ltrh"
alias k=kubectl
alias ct="ctags --options=$HOME/.ctags.d/terraform.ctags **/*.tf*"
alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox,.terraform} --exclude .zsh_history'
alias av="ansible-vault edit"
function pj() { source "$PROJECT_ROOT/.project" }

unset SSH_ASKPASS
export BAT_PAGER=''
export BAT_THEME='gruvbox-dark'
export SYSTEMD_PAGER=''
export VIRTUAL_ENV_DISABLE_PROMPT=1

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

if [ -f ~/.zshrc.local ]
then
    source ~/.zshrc.local
fi
