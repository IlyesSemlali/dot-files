# TODO: HISTORY config
## History file configuration
# [ -z "$HISTFILE" ] && HISTFILE="$HOME/.zsh_history"
# [ "$HISTSIZE" -lt 50000 ] && HISTSIZE=50000
# [ "$SAVEHIST" -lt 10000 ] && SAVEHIST=10000
SAVEHIST=2000
HISTSIZE=2000
HISTFILE=$HOME/.zsh_history

setopt HIST_IGNORE_DUPS SHARE_HISTORY HIST_FCNTL_LOCK
unsetopt menu_complete # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu # show completion menu on successive tab press
setopt complete_in_word
setopt always_to_end

# Changing/making/removing directory
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus

## History command configuration
setopt extended_history       # record timestamp of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_verify            # show command with history expansion to user before running it
setopt share_history          # share command history data
setopt multios                # enable redirect to multiple streams: echo >file1 >file2
setopt long_list_jobs         # show long list format job notifications
setopt interactivecomments    # recognize comments
