# vim: set ft=bash

TMUX_POWERLINE_SEPARATOR_LEFT_BOLD=""
TMUX_POWERLINE_SEPARATOR_LEFT_THIN=""
TMUX_POWERLINE_SEPARATOR_RIGHT_BOLD=""
TMUX_POWERLINE_SEPARATOR_RIGHT_THIN=""

TMUX_POWERLINE_DEFAULT_BACKGROUND_COLOR='236'
TMUX_POWERLINE_DEFAULT_FOREGROUND_COLOR='230'

TMUX_POWERLINE_DEFAULT_LEFTSIDE_SEPARATOR=$TMUX_POWERLINE_SEPARATOR_RIGHT_BOLD
TMUX_POWERLINE_DEFAULT_RIGHTSIDE_SEPARATOR=$TMUX_POWERLINE_SEPARATOR_LEFT_BOLD

# See man tmux.conf for additional formatting options for the status line.
# The `format regular` and `format inverse` functions are provided as conveinences

TMUX_POWERLINE_WINDOW_STATUS_CURRENT=(
  "#[$(format inverse)]" \
  "$TMUX_POWERLINE_SEPARATOR_RIGHT_BOLD" \
  " #W " \
  "#[$(format regular)]" \
  "$TMUX_POWERLINE_DEFAULT_LEFTSIDE_SEPARATOR"
)

if [ -z $TMUX_POWERLINE_WINDOW_STATUS_STYLE ]; then
	TMUX_POWERLINE_WINDOW_STATUS_STYLE=(
		"$(format regular)"
	)
fi

TMUX_POWERLINE_WINDOW_STATUS_FORMAT=(
  "#[$(format regular)]" \
  "$TMUX_POWERLINE_SEPARATOR_RIGHT_THIN" \
  " #W "
)

# Format: segment_name background_color foreground_color [non_default_separator]

TMUX_POWERLINE_LEFT_STATUS_SEGMENTS=(
  "project $TMUX_POWERLINE_DEFAULT_BACKGROUND_COLOR $TMUX_POWERLINE_DEFAULT_FOREGROUND_COLOR ${TMUX_POWERLINE_SEPARATOR_RIGHT_THIN}" \
  # "project $TMUX_POWERLINE_DEFAULT_BACKGROUND_COLOR $TMUX_POWERLINE_DEFAULT_FOREGROUND_COLOR" \
)

TMUX_POWERLINE_RIGHT_STATUS_SEGMENTS=(
  "vcs_branch $TMUX_POWERLINE_DEFAULT_BACKGROUND_COLOR $TMUX_POWERLINE_DEFAULT_FOREGROUND_COLOR ${TMUX_POWERLINE_SEPARATOR_LEFT_THIN}" \
  "time $TMUX_POWERLINE_DEFAULT_BACKGROUND_COLOR $TMUX_POWERLINE_DEFAULT_FOREGROUND_COLOR ${TMUX_POWERLINE_SEPARATOR_LEFT_THIN}" \
)

# Show which segment fails and its exit code.
export TMUX_POWERLINE_DEBUG_MODE_ENABLED="false"

# Use patched font symbols.
export TMUX_POWERLINE_PATCHED_FONT_IN_USE="true"

# The theme to use.
export TMUX_POWERLINE_THEME="default"

# Overlay directory to look for themes. There you can put your own themes outside the repo. Fallback will still be the "themes" directory in the repo.
export TMUX_POWERLINE_DIR_USER_THEMES="${HOME}/.config/tmux-powerline/themes/"
# Overlay directory to look for segments. There you can put your own segments outside the repo. Fallback will still be the "segments" directory in the repo.
export TMUX_POWERLINE_DIR_USER_SEGMENTS="${HOME}/.config/tmux-powerline/segments/"

# Only use the session name
export TMUX_POWERLINE_SEG_TMUX_SESSION_INFO_FORMAT='#S'

# Use short or long format for the hostname. Can be {"short, long"}.
export TMUX_POWERLINE_SEG_HOSTNAME_FORMAT="short"
