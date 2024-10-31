# HomeBrew
if [ -f ${HOME}/.local/share/homebrew/bin/brew ]
then
  eval $(${HOME}/.local/share/homebrew/bin/brew shellenv)
else
  echo "init-home: brew not installed, please run 'init-home -b'"
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

