export PATH=$HOME/.local/bin:$PATH
export PATH="$PATH:/usr/local/lib64/ruby/gems/2.5.0/bin/"

# Change this to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Oh-My-ZSH Theme
ZSH_THEME="agnoster"

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

# Saves lasts CWD into a stack and start new instances
# in the last CWD
autoload -Uz add-zsh-hook

DIRSTACKFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zshdirs"
if [[ -f "$DIRSTACKFILE" ]] && (( ${#dirstack} == 0 ))
then
	if echo $TTY | grep -q pts
	then
		dirstack=("${(@f)"$(< "$DIRSTACKFILE")"}")
		[[ -d "${dirstack[1]}" ]] && cd -- "${dirstack[1]}"
	fi
fi

chpwd_dirstack() {
	if echo $TTY | grep -q pts
	then
		print -l -- "$PWD" "${(u)dirstack[@]}" > "$DIRSTACKFILE"
	fi
}

add-zsh-hook -Uz chpwd chpwd_dirstack

DIRSTACKSIZE='20'

setopt AUTO_PUSHD PUSHD_SILENT PUSHD_TO_HOME
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_MINUS

# # Use ESC to edit currunt command line in VIM
# export KEYTIMEOUT=1
# autoload -U edit-command-line
# zle -N edit-command-line
# bindkey '\033' edit-command-line

precmd() {
	if git status > /dev/null 2>&1
	then
		export PROJECT_ROOT="$(git rev-parse --show-toplevel)"
		# export DIRSTACKFILE="$PROJECT_ROOT/.dirstack"
		touch $DIRSTACKFILE
		fc -R
		export HISTFILE="$PROJECT_ROOT/.zsh_history"
		if which nvim > /dev/null 2>&1
		then
			touch "$PROJECT_ROOT/.vim_session"
			alias vim="nvim -S $PROJECT_ROOT/.vim_session"
		fi
	fi
}

alias dv="dirs -v"
alias ll="ls -ltrh"
alias git="git --no-pager"
alias k=kubectl
alias ct="ctags --options=$HOME/.ctags.d/terraform.ctags **/*.tf*"
alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn,.idea,.tox,.terraform} --exclude .zsh_history'

if which nvim >/dev/null 2>&1
then
	alias vim="nvim"
fi

unset SSH_ASKPASS
export EDITOR=vim
export BAT_PAGER=''
export SYSTEMD_PAGER=''
export DISABLE_UPDATE_PROMPT=true
export QT_QPA_PLATFORMTHEME="qt5ct"

# Prompt

prompt_sign () {
	echo -n '\n\n> '
}

prompt_dir () {
	prompt_segment white $CURRENT_FG '%~'
}

prompt_begin () {
	# TODO: Find a way to remove blank line on first run
	echo -n '\n'
}

if [[ $TERM == "linux" ]]
then
	export SEGMENT_SEPARATOR=')'
else
	export SEGMENT_SEPARATOR='\ue0b4'
fi

build_prompt() {
  RETVAL=$?
  prompt_begin
  prompt_status
  prompt_context
  prompt_git
  prompt_bzr
  prompt_hg
  prompt_virtualenv
  prompt_dir
  prompt_end
  prompt_sign
}


autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /home/ilyes/bin/terraform-0.15.3 terraform-0.15.3
