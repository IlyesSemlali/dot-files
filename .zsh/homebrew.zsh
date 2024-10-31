# HomeBrew
if [ -f ${HOME}/.local/share/homebrew/bin/brew ]
then
  eval $(${HOME}/.local/share/homebrew/bin/brew shellenv)
  insert_path "$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin"
else
  echo "init-home: brew not installed, please run '~/.init-home'"
fi
