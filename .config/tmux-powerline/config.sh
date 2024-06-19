# Default configuration file for tmux-powerline.
# Modeline {
#	 vi: foldmarker={,} foldmethod=marker foldlevel=0 tabstop=4 filetype=sh
# }

# General
	# Show which segment fails and its exit code.
	export TMUX_POWERLINE_DEBUG_MODE_ENABLED="false"
	# Use patched font symbols.
	export TMUX_POWERLINE_PATCHED_FONT_IN_USE="true"

	# The theme to use.
	export TMUX_POWERLINE_THEME="dark"
	# Overlay directory to look for themes. There you can put your own themes outside the repo. Fallback will still be the "themes" directory in the repo.
	export TMUX_POWERLINE_DIR_USER_THEMES="${XDG_CONFIG_HOME:-$HOME/.config}/tmux-powerline/themes"
	# Overlay directory to look for segments. There you can put your own segments outside the repo. Fallback will still be the "segments" directory in the repo.
	export TMUX_POWERLINE_DIR_USER_SEGMENTS="${XDG_CONFIG_HOME:-$HOME/.config}/tmux-powerline/segments"

	# The initial visibility of the status bar. Can be {"on", "off", "2"}. 2 will create two status lines: one for the window list and one with status bar segments.
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

	# Uncomment these if you want to enable tmux bindings for muting (hiding) one of the status bars.
	# E.g. this example binding would mute the left status bar when pressing <prefix> followed by Ctrl-[
	#export TMUX_POWERLINE_MUTE_LEFT_KEYBINDING="C-["
	#export TMUX_POWERLINE_MUTE_RIGHT_KEYBINDING="C-]"


# battery.sh {
	# How to display battery remaining. Can be {percentage, cute}.
	export TMUX_POWERLINE_SEG_BATTERY_TYPE="percentage"
	# How may hearts to show if cute indicators are used.
	export TMUX_POWERLINE_SEG_BATTERY_NUM_HEARTS="5"
# }

# date.sh {
	# date(1) format for the date. If you don't, for some reason, like ISO 8601 format you might want to have "%D" or "%m/%d/%Y".
	export TMUX_POWERLINE_SEG_DATE_FORMAT="%F"
# }

# hostname.sh {
	# Use short or long format for the hostname. Can be {"short, long"}.
	export TMUX_POWERLINE_SEG_HOSTNAME_FORMAT="short"
# }

# kubernetes_context.sh {
	# Kubernetes config context display mode {"name_namespace", "name", "namespace"}.
	# export TMUX_POWERLINE_SEG_KUBERNETES_CONTEXT_DISPLAY_MODE="name_namespace"
	# Kubernetes config context symbol.
	# export TMUX_POWERLINE_SEG_KUBERNETES_CONTEXT_SYMBOL="󱃾"
	# Kubernetes config context symbol colour.
	# export TMUX_POWERLINE_SEG_KUBERNETES_CONTEXT_SYMBOL_COLOUR="255"
	# Separator for display mode "name_namespace"
	# TMUX_POWERLINE_SEG_KUBERNETES_CONTEXT_SEPARATOR="󰿟"
# }

# mode_indicator.sh {
	# Whether the normal & prefix mode section should be enabled. Should be {"true, "false"}.
	export TMUX_POWERLINE_SEG_MODE_INDICATOR_NORMAL_AND_PREFIX_MODE_ENABLED="true"
	# Normal mode text & color overrides. Defaults to "normal" & the segment foreground color set in the theme used.
	export TMUX_POWERLINE_SEG_MODE_INDICATOR_NORMAL_MODE_TEXT="normal"
	export TMUX_POWERLINE_SEG_MODE_INDICATOR_NORMAL_MODE_TEXT_COLOR=""
	# Prefix mode text & color overrides. Defaults to "prefix" & the segment foreground color set in the theme used.
	export TMUX_POWERLINE_SEG_MODE_INDICATOR_PREFIX_MODE_TEXT="prefix"
	export TMUX_POWERLINE_SEG_MODE_INDICATOR_PREFIX_MODE_TEXT_COLOR=""
	# Whether the mouse mode section should be enabled. Should be {"true, "false"}.
	export TMUX_POWERLINE_SEG_MODE_INDICATOR_MOUSE_MODE_ENABLED="true"
	# Mouse mode text & color overrides. Defaults to "mouse" & the segment foreground color set in the theme used.
	export TMUX_POWERLINE_SEG_MODE_INDICATOR_MOUSE_MODE_TEXT="mouse"
	export TMUX_POWERLINE_SEG_MODE_INDICATOR_MOUSE_MODE_TEXT_COLOR=""
	# Whether the copy mode section should be enabled. Should be {"true, "false"}.
	export TMUX_POWERLINE_SEG_MODE_INDICATOR_COPY_MODE_ENABLED="true"
	# Copy mode text & color overrides. Defaults to "copy" & the segment foreground color set in the theme used.
	export TMUX_POWERLINE_SEG_MODE_INDICATOR_COPY_MODE_TEXT="copy"
	export TMUX_POWERLINE_SEG_MODE_INDICATOR_COPY_MODE_TEXT_COLOR=""
	# Suspend mode text & color overrides. Defaults to "SUSPEND" & the segment foreground color set in the theme used.
	export TMUX_POWERLINE_SEG_MODE_INDICATOR_SUSPEND_MODE_TEXT="SUSPEND"
	export TMUX_POWERLINE_SEG_MODE_INDICATOR_SUSPEND_MODE_TEXT_COLOR=""
	# Separator text override. Defaults to " • ".
	export TMUX_POWERLINE_SEG_MODE_INDICATOR_SEPARATOR_TEXT=" • "
# }

# now_playing.sh {
	# Music player to use. Can be any of {audacious, banshee, cmus, apple_music, itunes, lastfm, plexamp, mocp, mpd, mpd_simple, pithos, playerctl, rdio, rhythmbox, spotify, spotify_wine, file}.
	export TMUX_POWERLINE_SEG_NOW_PLAYING_MUSIC_PLAYER="spotify"
	# File to be read in case the song is being read from a file
	export TMUX_POWERLINE_SEG_NOW_PLAYING_FILE_NAME=""
	# Maximum output length.
	export TMUX_POWERLINE_SEG_NOW_PLAYING_MAX_LEN="40"
	# How to handle too long strings. Can be {trim, roll}.
	export TMUX_POWERLINE_SEG_NOW_PLAYING_TRIM_METHOD="trim"
	# Characters per second to roll if rolling trim method is used.
	export TMUX_POWERLINE_SEG_NOW_PLAYING_ROLL_SPEED="2"
	# Mode of roll text {"space", "repeat"}. space: fill up with empty space; repeat: repeat text from beginning
	# export TMUX_POWERLINE_SEG_NOW_PLAYING_ROLL_MODE="repeat"
	# Separator for "repeat" roll mode
	# export TMUX_POWERLINE_SEG_NOW_PLAYING_ROLL_SEPARATOR="   "
	# If set to 'true', 'yes', 'on' or '1', played tracks will be logged to a file.
	# export TMUX_POWERLINE_SEG_NOW_PLAYING_TRACK_LOG_ENABLE="false"
	# If enabled, log played tracks to the following file:
	# export TMUX_POWERLINE_SEG_NOW_PLAYING_TRACK_LOG_FILEPATH="/Users/ilyes/.now_playing.log"
	# Maximum number of logged song entries. Set to "unlimited" for unlimited entries.
	# export TMUX_POWERLINE_SEG_NOW_PLAYING_TRACK_LOG_MAX_ENTRIES="100"

	# Hostname for MPD server in the format "[password@]host"
	export TMUX_POWERLINE_SEG_NOW_PLAYING_MPD_HOST="localhost"
	# Port the MPD server is running on.
	export TMUX_POWERLINE_SEG_NOW_PLAYING_MPD_PORT="6600"
	# Song display format for mpd_simple. See mpc(1) for delimiters.
	export TMUX_POWERLINE_SEG_NOW_PLAYING_MPD_SIMPLE_FORMAT="%artist% - %title%"
	# Song display format for playerctl. see "Format Strings" in playerctl(1).
	export TMUX_POWERLINE_SEG_NOW_PLAYING_PLAYERCTL_FORMAT="{{ artist }} - {{ title }}"
	# Song display format for rhythmbox. see "FORMATS" in rhythmbox-client(1).
	export TMUX_POWERLINE_SEG_NOW_PLAYING_RHYTHMBOX_FORMAT="%aa - %tt"

	# Last.fm
	# Set up steps for Last.fm
	# 1. Make sure jq(1) is installed on the system.
	# 2. Create a new API application at https://www.last.fm/api/account/create (name it tmux-powerline) and copy the API key and insert it below in the setting TMUX_POWERLINE_SEG_NOW_PLAYING_LASTFM_API_KEY
	# 3. Make sure the API can access your recently played song by going to you user privacy settings https://www.last.fm/settings/privacy and make sure "Hide recent listening information" is UNCHECKED.
	# Username for Last.fm if that music player is used.
	export TMUX_POWERLINE_SEG_NOW_PLAYING_LASTFM_USERNAME=""
	# API Key for the API.
	export TMUX_POWERLINE_SEG_NOW_PLAYING_LASTFM_API_KEY=""
	# How often in seconds to update the data from last.fm.
	export TMUX_POWERLINE_SEG_NOW_PLAYING_LASTFM_UPDATE_PERIOD="30"
	# Fancy char to display before now playing track
	export TMUX_POWERLINE_SEG_NOW_PLAYING_NOTE_CHAR="♫"

	# Plexamp
	# Set up steps for Plexamp
	# 1. Make sure jq(1) is installed on the system.
	# 2. Make sure you have an instance of Tautulli that is accessible by the computer running tmux-powerline.
	# Username for Plexamp if that music player is used.
	export TMUX_POWERLINE_SEG_NOW_PLAYING_PLEXAMP_USERNAME=""
	# Hostname for Tautulli server in the format "[password@]host"
	export TMUX_POWERLINE_SEG_NOW_PLAYING_PLEXAMP_TAUTULLI_HOST=""
	# API Key for Tautulli.
	export TMUX_POWERLINE_SEG_NOW_PLAYING_PLEXAMP_TAUTULLI_API_KEY=""
	# How often in seconds to update the data from Plexamp.
	export TMUX_POWERLINE_SEG_NOW_PLAYING_PLEXAMP_UPDATE_PERIOD="30"
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
	export TMUX_POWERLINE_SEG_VCS_BRANCH_GIT_SYMBOL_COLOUR="255"


# vcs_modified.sh {
	# Symbol for count of modified vcs files.
	# export TMUX_POWERLINE_SEG_VCS_MODIFIED_SYMBOL="± "
# }

# vcs_rootpath.sh {
	# Display mode for vcs_rootpath.
	# Example: (name: folder name only; path: full path, w/o expansion; user_path: full path, w/ tilde expansion)
	# export TMUX_POWERLINE_SEG_VCS_ROOTPATH_MODE="name"
# }

# vcs_staged.sh {
	# Symbol for count of staged vcs files.
	# export TMUX_POWERLINE_SEG_VCS_STAGED_SYMBOL="⊕ "
# }
