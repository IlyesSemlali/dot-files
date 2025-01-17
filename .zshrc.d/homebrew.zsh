# HomeBrew
if [ -f ${HOME}/.local/share/homebrew/bin/brew ]
then
  eval $(${HOME}/.local/share/homebrew/bin/brew shellenv)
  insert_path "$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin"
else
  log_error "homebrew" "homebrew not installed, please run '~/.init-home'"
fi
