
function insert_path() {
    if ! echo $PATH | grep -q "$1:"
    then
        PATH="$1:$PATH"
    fi
}

function append_path() {
    if ! echo $PATH | grep -q ":$1"
    then
        PATH="$PATH:$1"
    fi
}
