# Default configuration file for tmux-powerline.
# Modeline {
#	 vi: foldmarker={,} foldmethod=marker foldlevel=0 tabstop=4 filetype=sh
# }

# General
# The theme to use.
export TMUX_POWERLINE_THEME="monochrome"

# Show which segment fails and its exit code.
export TMUX_POWERLINE_DEBUG_MODE_ENABLED="false"

# Use patched font symbols.
export TMUX_POWERLINE_PATCHED_FONT_IN_USE="true"

# Overlay directory to look for themes. There you can put your own themes outside the repo. Fallback will still be the "themes" directory in the repo.
export TMUX_POWERLINE_DIR_USER_THEMES="${XDG_CONFIG_HOME:-$HOME/.config}/tmux-powerline/themes"

# Overlay directory to look for segments. There you can put your own segments outside the repo. Fallback will still be the "segments" directory in the repo.
export TMUX_POWERLINE_DIR_USER_SEGMENTS="${XDG_CONFIG_HOME:-$HOME/.config}/tmux-powerline/segments"

# Initial visibility of the status bar. Can be {"on", "off", "2"}. 2 will create two status lines: one for the window list and one with status bar segments.
export TMUX_POWERLINE_STATUS_VISIBILITY="on"

# In case of visibility = 2, where to display window status and where left/right status bars.
# 0: window status top, left/right status bottom; 1: window status bottom, left/right status top
export TMUX_POWERLINE_WINDOW_STATUS_LINE=0

# The status bar refresh interval in seconds.
# Note that events that force-refresh the status bar (such as window renaming) will ignore this.
export TMUX_POWERLINE_STATUS_INTERVAL="1"

# The location of the window list. Can be {"absolute-centre, centre, left, right"}.
# Note that "absolute-centre" is only supported on `tmux -V` >= 3.2.
export TMUX_POWERLINE_STATUS_JUSTIFICATION="left"

# The maximum length of the left status bar.
export TMUX_POWERLINE_STATUS_LEFT_LENGTH="60"

# The maximum length of the right status bar.
export TMUX_POWERLINE_STATUS_RIGHT_LENGTH="90"

# The separator to use between windows on the status bar.
export TMUX_POWERLINE_WINDOW_STATUS_SEPARATOR=""

# date.sh {fold
# date(1) format for the date. If you don't, for some reason, like ISO 8601 format you might want to have "%D" or "%m/%d/%Y".
export TMUX_POWERLINE_SEG_DATE_FORMAT="%F"
# }

# time.sh {
# date(1) format for the time. Americans might want to have "%I:%M %p".
export TMUX_POWERLINE_SEG_TIME_FORMAT="%H:%M"
# Change this to display a different timezone than the system default.
# Use TZ Identifier like "America/Los_Angeles"
# export TMUX_POWERLINE_SEG_TIME_TZ=""
# }

# vcs_branch.sh
# Max length of the branch name.
export TMUX_POWERLINE_SEG_VCS_BRANCH_MAX_LEN=""
# Branch symbol for git repositories
export TMUX_POWERLINE_SEG_VCS_BRANCH_DEFAULT_SYMBOL=""
# export TMUX_POWERLINE_SEG_VCS_BRANCH_GIT_SYMBOL="${TMUX_POWERLINE_SEG_VCS_BRANCH_DEFAULT_SYMBOL}"
# Branch symbol colour for git repositories
# export TMUX_POWERLINE_SEG_VCS_BRANCH_GIT_SYMBOL_COLOUR="7"
