
function insert_path() {
    if ! echo $PATH | grep -q "$1:"
    then
        PATH="$1:$PATH"
    else
        PATH="$1:$(echo $PATH | sed "s@$1:@@g")"
    fi
}

function append_path() {
    if ! echo $PATH | grep -q ":$1"
    then
        PATH="$PATH:$1"
    else
        PATH="$(echo $PATH | sed "s@:$1@@g"):$1"
    fi
}
