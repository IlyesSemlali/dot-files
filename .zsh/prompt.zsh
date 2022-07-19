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
        if [[ -d .terraform ]]
        then
            local tf_workspace=$(terraform workspace show)
            prompt_segment blue $CURRENT_FG $tf_workspace
        fi
    fi
}

prompt_kube () {
    if which kubectl > /dev/null 2>&1
    then
        if [[ -n $ZPROMPT_SHOW_KUBE ]]
        then
            local kube_context=$(kubectl config current-context)
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
    RETVAL=$?
    prompt_begin
    prompt_status
    prompt_context
    prompt_git
    prompt_bzr
    prompt_hg
    prompt_tf
    prompt_kube
    prompt_virtualenv
    prompt_dir
    prompt_end
    prompt_sign
}
