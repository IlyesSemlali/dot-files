# shellcheck shell=bash
# Shows project name

TMUX_POWERLINE_SEG_PROJECT_STRIP_PATTERN="${TMUX_POWERLINE_SEG_PROJECT_STRIP_PATTERN:-%F}"

PROJECT_CACHE="${HOME}/.cache/project-switcher"

generate_segmentrc() {
	read -d '' rccontents  << EORC
# This is a sed filter, it will be applied to the '\$PROJECT' variable to remove unneeded characters.
export TMUX_POWERLINE_SEG_PROJECT_STRIP_PATTERN="${TMUX_POWERLINE_SEG_PROJECT_STRIP_PATTERN_DEFAULT}"
EORC
	echo "$rccontents"
}

run_segment() {
  echo "tmux" # TODO: find a way to print the project

	return 0
}
