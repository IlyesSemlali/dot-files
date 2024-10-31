source ${HOME}/.zsh/plugin-manager.zsh
source ${HOME}/.zsh/utils.zsh
source ${HOME}/.zsh/options.zsh
source ${HOME}/.zsh/homebrew.zsh

##################### Beginning of OMZ #####################

# TODO: use the -g alias flag for work related stuff (project-management for instance)
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'


# TODO: rework those ls aliases for me
# List directory contents
alias lsa='ls -lah'
alias l='ls -lah'
alias ll='ls -lh'
alias la='ls -lAh'

# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html
# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Zle-Builtins
# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Standard-Widgets

# TODO: maybe something useful here !
# Make sure that the terminal is in application mode when zle is active, since
# only then values from $terminfo are valid
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
  function zle-line-init() {
    echoti smkx
  }
  function zle-line-finish() {
    echoti rmkx
  }
  zle -N zle-line-init
  zle -N zle-line-finish
fi


# Use diff --color if available
if command diff --color /dev/null{,} &>/dev/null; then
  function diff {
    command diff --color "$@"
  }
fi

##################### End of OMZ #####################



###############################
##### Aliases and exports #####
###############################

if which nvim >/dev/null 2>&1
then
  alias ls="eza --icons --level 1"
  alias l="eza -l --level 1 --git --no-permissions"
  alias ll="eza -al --level 1 --git-ignore --git"
else
  alias ls="ls --color=auto"
  alias l="ls --full-time -ltrh"
  alias ll="ls --full-time -altrh"
fi

alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox,.terraform} --exclude .zsh_history'
alias dv="cdi"
alias pj="sd project"
alias cpj="cd ${HOME}/projects/$PROJECT/"

unset SSH_ASKPASS

export SYSTEMD_PAGER=''
export VIRTUAL_ENV_DISABLE_PROMPT=1

export FZF_DEFAULT_OPTS="--height=25 --min-height=15 --border=rounded --margin=1 --padding=1 --border-label-pos=3"

export K9S_CONFIG_DIR="$HOME/.config/k9s/"

export _ZO_FZF_OPTS="--border-label='Change Directory' --preview 'eza -al --group-directories-first --no-user --no-time --no-filesize --no-permissions {2..}' --preview-window right,40% --height 40% --reverse --ansi"

export SD_ROOT="${HOME}/.local/sd"
export SD_CAT="bat"

export GOPATH="${HOME}/.local/share/go"

###############################


#####################################
##### Shell customization tools #####
#####################################

# Starship prompt
(( $+commands[starship] )) && eval "$(starship init zsh)" || export PS1="-> "

# Fuzzy-find CD replacement
(( $+commands[zoxide] )) && eval "$(zoxide init --cmd cd zsh)"

# Themes for ls and tree using sharkdp/vivid
(( $+commands[vivid] )) && export LS_COLORS="$(vivid generate ${HOME}/.themes/gruvbox-dark/vivid.yaml)" 2>/dev/null

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


if [ -f ${HOME}/.zshrc.d/local.zsh ]
then
  source ${HOME}/.zshrc.d/local.zsh
fi

source ${HOME}/.zsh/completion.zsh
source ${HOME}/.zsh/keybindings.zsh
source ${HOME}/.zshrc.tools
source ${HOME}/.zshrc.d/project-management.zsh
