source "${HOME}/.local/lib/log"

source ${HOME}/.zsh/utils.zsh
source ${HOME}/.zshrc.d/plugin-manager.zsh
source ${HOME}/.zshrc.d/options.zsh
source ${HOME}/.zshrc.d/homebrew.zsh

###############################
##### Aliases and exports #####
###############################

if which eza >/dev/null 2>&1
then
  alias ls="eza --icons --level 1"
  alias  l="eza --git-ignore --git --no-user --no-permissions --level 1 -l"
  alias ll="eza --git --no-user --no-permissions --level 1 -l"
  alias la="eza --git-ignore --git --level 1 -al"
else
  alias l='ls --color=auto -lh'
  alias ll='ls --color=auto -lah'
fi

alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox,.terraform} --exclude .zsh_history'
alias dv="cdi"
alias pj="sd project"
alias cpj="cd ${HOME}/projects/$PROJECT/"
alias -g spj="source ${HOME}/projects/$PROJECT/.project.d/zshrc"

unset SSH_ASKPASS

export SYSTEMD_PAGER=''
export VIRTUAL_ENV_DISABLE_PROMPT=1

export FZF_DEFAULT_OPTS="--height=25 --min-height=15 --border=rounded --margin=1 --padding=1 --border-label-pos=3"

export K9S_CONFIG_DIR="$HOME/.config/k9s/"

export _ZO_FZF_OPTS="--border-label='Change Directory' --preview 'eza -al --group-directories-first --no-user --no-time --no-filesize --no-permissions {2..}' --preview-window right,40% --height 40% --reverse --ansi"

export SD_ROOT="${HOME}/.local/sd"
export SD_CAT="bat"

export GOPATH="${HOME}/.local/share/go"

export PRE_COMMIT_ALLOW_NO_CONFIG=1

###############################


#####################################
##### Shell customization tools #####
#####################################

# Starship prompt
(( $+commands[starship] )) && eval "$(starship init zsh)" || export PS1="-> "

# Fuzzy-find CD replacement
(( $+commands[zoxide] )) && eval "$(zoxide init --cmd cd zsh)"

# Themes for ls and tree using sharkdp/vivid
(( $+commands[vivid] )) && export LS_COLORS="$(vivid generate ${HOME}/.themes/gruvbox-dark/vivid.yaml)"

#####################################

#NeoVIM
if which nvim >/dev/null 2>&1
then
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


if [ -f ${HOME}/.zshrc.d/local.zsh ]
then
  source ${HOME}/.zshrc.d/local.zsh
fi

source ${HOME}/.zshrc.d/completion.zsh
source ${HOME}/.zshrc.d/keybindings.zsh
source ${HOME}/.zshrc.d/project-management.zsh
