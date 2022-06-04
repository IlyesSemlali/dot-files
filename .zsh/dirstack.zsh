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

precmd() {
    if git status > /dev/null 2>&1
    then
        export PROJECT_ROOT="$(git rev-parse --show-toplevel)"
        # export DIRSTACKFILE="$PROJECT_ROOT/.dirstack"
        mkdir -p $PROJECT_ROOT
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

dv() {
    cd $(cat $DIRSTACKFILE | fzf)
}
