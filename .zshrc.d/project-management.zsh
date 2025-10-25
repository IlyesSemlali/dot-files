# Project management

if [[ -v PROJECT ]]; then
	export _ZO_DATA_DIR="${HOME}/projects/$PROJECT/.project.d"
	export HISTFILE="${HOME}/projects/$PROJECT/.project.d/zsh_history"

	if [[ -f "${HOME}/projects/$PROJECT/.project.d/zshrc" ]]; then
		source "${HOME}/projects/$PROJECT/.project.d/zshrc"
	fi

fi

if [ -f ${HOME}/project/$PROJECT/.project ]; then
	source ${HOME}/project/$PROJECT/.project
fi

# TODO: understand how compdef works to implement it later
# Source: OMZ
# function d () {
#   if [[ -n $1 ]]; then
#     dirs "$@"
#   else
#     dirs -v | head -n 10
#   fi
# }
# compdef _dirs d

# TODO: use the -g alias flag project-management
# alias -g .pj='~/projects/${PROJECT}'
