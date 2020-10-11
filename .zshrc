export PATH=$HOME/.local/bin:$PATH

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

<<<<<<< HEAD
source ~/.zshrc.d/*.zsh
||||||| parent of 08434e6 (added powerline fonts and config in vimrc)
if [ -f .zshrc.local ]
then
	source .zshrc.local
fi
=======
if [ -f ~/.zshrc.local ]
then
	source ~/.zshrc.local
fi
>>>>>>> 08434e6 (added powerline fonts and config in vimrc)
