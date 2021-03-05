PROMPT="%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ )"

PROMPT+=' %{$fg[cyan]%}%c%{$reset_color%} $( [ -v GIT_PROMPT_EXCLUDES ] && (( ${GIT_PROMPT_EXCLUDES[(I)$(git_repo_name)]} )) || git_prompt_info)'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[blue]%}git:(%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}✗"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"

[[ ${UID} -eq 0 ]] && PROMPT_PREFIX='(%n)'
(( ${+SSH_TTY} )) && PROMPT_PREFIX='(%n@%m)'
PROMPT="$PROMPT_PREFIX $PROMPT"

RPROMPT='%*'
