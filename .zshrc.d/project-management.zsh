# Project management

export ACTIVE_ORGANIZATION="$(cat ${HOME}/.cache/project/organization)"
export PROJECT_DIR="${HOME}/projects/${ACTIVE_ORGANIZATION}/${PROJECT}"

alias pj="sd project"
alias cpj="cd ${HOME}/projects/${ACTIVE_ORGANIZATION}/${PROJECT}/"
alias -g spj="source ${HOME}/projects/${ACTIVE_ORGANIZATION}/${PROJECT}/.project.d/zshrc"

if [[ -v PROJECT ]]; then
	export _ZO_DATA_DIR="${HOME}/projects/${ACTIVE_ORGANIZATION}/${PROJECT}/.project.d"
	export HISTFILE="${HOME}/projects/${ACTIVE_ORGANIZATION}/${PROJECT}/.project.d/zsh_history"

	if [[ -f "${HOME}/projects/${ACTIVE_ORGANIZATION}/${PROJECT}/.project.d/zshrc" ]]; then
		source "${HOME}/projects/${ACTIVE_ORGANIZATION}/${PROJECT}/.project.d/zshrc"
	fi

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
