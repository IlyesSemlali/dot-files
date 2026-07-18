# Mise-en-Place (jdx/mise)
if which mise >/dev/null 2>&1; then
	eval "$(mise activate zsh)"
fi

if  -x "$(command -v any-nix-shell)" ; then
  any-nix-shell zsh --info-right | source /dev/stdin
fi
