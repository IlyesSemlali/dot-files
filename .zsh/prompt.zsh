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
