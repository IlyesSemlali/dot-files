export PATH=$HOME/.local/bin:$PATH

export PATH="$PATH:/usr/local/lib64/ruby/gems/2.5.0/bin/"

# Change this to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Oh-My-ZSH Theme
#ZSH_THEME="robbyrussell"
ZSH_THEME="robberyssull"

# Custom parameter to prevent folder from showing in the prompt
GIT_PROMPT_EXCLUDES=($USER)

# Oh-My-ZSH Plugins
plugins=(git docker docker-compose)
autoload -U compinit && compinit

if [ -f ~/.zshrc.local ]
then
	source ~/.zshrc.local
fi

source $ZSH/oh-my-zsh.sh

# needs merge
alias ll='ls -ltrh'
alias git="git --no-pager"
alias k=kubectl

unset SSH_ASKPASS
export EDITOR=vim
export SYSTEMD_PAGER=''
export DISABLE_UPDATE_PROMPT=true
export QT_QPA_PLATFORMTHEME="qt5ct"

# Saves lasts CWD into a stack and start new instances
# in the last CWD
autoload -Uz add-zsh-hook

DIRSTACKFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zshdirs"
if [[ -f "$DIRSTACKFILE" ]] && (( ${#dirstack} == 0 )); then
	dirstack=("${(@f)"$(< "$DIRSTACKFILE")"}")
	[[ -d "${dirstack[1]}" ]] && cd -- "${dirstack[1]}"
fi
chpwd_dirstack() {
	print -l -- "$PWD" "${(u)dirstack[@]}" > "$DIRSTACKFILE"
}
add-zsh-hook -Uz chpwd chpwd_dirstack

DIRSTACKSIZE='20'

setopt AUTO_PUSHD PUSHD_SILENT PUSHD_TO_HOME

## Remove duplicate entries
setopt PUSHD_IGNORE_DUPS

## This reverts the +/- operators.
setopt PUSHD_MINUS
