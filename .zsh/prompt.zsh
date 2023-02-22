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

prompt_tf () {
    if which terraform > /dev/null 2>&1
    then
        if [[ -d .terraform ]] && history | tail -30 | grep -v 'grep' | grep -q ' tf ' > /dev/null 2>&1
        then
            local tf_workspace=$(terraform workspace show)
            prompt_segment blue $CURRENT_FG $tf_workspace
        fi
    fi
}

prompt_kube () {
    if which kubectl > /dev/null 2>&1
    then
        if history | tail -30 | grep -v 'grep' | grep -q '\ \(k\|kubectl\)\ ' > /dev/null 2>&1
        then
            local kube_context=$(kubectl config current-context) 2>/dev/null
            prompt_segment cyan $CURRENT_FG $kube_context
        fi
    fi
}

prompt_virtualenv () {
    [[ -n ${VIRTUAL_ENV} ]] || return
    prompt_segment red $CURRENT_FG $(basename "$VIRTUAL_ENV")
}

if [[ $TERM == "linux" ]]
then
    export SEGMENT_SEPARATOR=')'
else
    export SEGMENT_SEPARATOR='\ue0b0'
fi

build_prompt () {
    prompt_begin

    if [ -z "$DEMO_MODE" ]; then
        RETVAL=$?
        prompt_status

        # Shown in tmux, only print when outside of a session
        if [ -z "$TMUX" ]; then
            prompt_context
        fi

        if git rev-parse --is-inside-git-dir > /dev/null 2>&1 ; then
            git_dirty=$(parse_git_dirty)
            if [[ -n ${git_dirty} ]]; then
                if [[ "$(git rev-parse --show-toplevel | tr '[:upper:]' '[:lower:]')" != "$(echo ${HOME}) | tr '[:upper:]' '[:lower:]')" ]]; then
                    prompt_git
                fi
            fi
        fi

        # Only show in a TMUX session
        if [ -n "$TMUX" ]; then
            prompt_tf
            prompt_kube
        fi

        prompt_dir

        prompt_end
        prompt_sign
    else
        prompt_sign
    fi
}
