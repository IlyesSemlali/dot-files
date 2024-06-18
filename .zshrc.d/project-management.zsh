# Project management

if [[ -v PROJECT ]]; then
  export _ZO_DATA_DIR="${HOME}/projects/$PROJECT/.project.d"
  export HISTFILE="${HOME}/projects/$PROJECT/.project.d/zsh_history"
  if [[ -f "${HOME}/projects/$PROJECT/.project.d/zshrc" ]]
  then
    source "${HOME}/projects/$PROJECT/.project.d/zshrc"
  fi
fi

if [ -f ${HOME}/project/$PROJECT/.project ]
then
    source ${HOME}/project/$PROJECT/.project
fi

