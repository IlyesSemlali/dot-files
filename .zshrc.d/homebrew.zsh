# HomeBrew
if [ -f ${HOME}/.local/share/homebrew/bin/brew ]
then
  eval $(${HOME}/.local/share/homebrew/bin/brew shellenv)
  insert_path "$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin"
else
  log_error "homebrew" "homebrew not installed, please run '~/.init-home'"
fi

# Add paths to GNU tools using cache
gnupaths="${HOME}/.cache/gnupaths"

if [ ! -e ${gnupaths} ]; then
    find ${HOME}/.local/share/homebrew/opt -type d -follow -name gnubin -print > ${gnupaths}
fi

for bindir in $(cat ${gnupaths}); do
    insert_path "${bindir}"
done

unset gnupaths

