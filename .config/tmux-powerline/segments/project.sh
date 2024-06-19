# shellcheck shell=bash
# Shows project name

TMUX_POWERLINE_SEG_PROJECT_STRIP_PATTERN="${TMUX_POWERLINE_SEG_PROJECT_STRIP_PATTERN}"

generate_segmentrc() {
	read -d '' rccontents  << EORC
# This is a sed filter, it will be applied to the '\$PROJECT' variable to remove unneeded characters.
export TMUX_POWERLINE_SEG_PROJECT_STRIP_PATTERN="${TMUX_POWERLINE_SEG_PROJECT_STRIP_PATTERN_DEFAULT}"
EORC
	echo "$rccontents"
}

run_segment() {
        if [ -z $TMUX_POWERLINE_SEG_PROJECT_STRIP_PATTERN ];
        then
          tmux display-message -p '#S' | sed 's/-[0-9]*$//'
        else
          tmux display-message -p '#S' | sed 's/-[0-9]*$//' | sed "s/$TMUX_POWERLINE_SEG_PROJECT_STRIP_PATTERN//"
        fi
        return 0
}
