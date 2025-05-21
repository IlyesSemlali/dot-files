# Load compinit only once
command -v compinit >/dev/null || {
	autoload -Uz +X compinit && compinit -u
}


# Complete . and .. special directories
zstyle ':completion:*' special-dirs true

# # Disable named-directories autocompletion
# zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories

# Use caching so that long-running commands are useable
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' cache-path $ZSH_CACHE_DIR

# Match partial words, case insensitively
zstyle ':completion:*' matcher-list \
    'm:{[:lower:]}={[:upper:]}' \
    '+r:|[._-]=* r:|=*' \
    '+l:|=*'

# Zsh to use the same colors as ls
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}  # zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

# Show selected completion item
zstyle ':completion:*:*:*:*:*' menu select

export ZSH_HIGHLIGHT_STYLES[path]='fg=cyan'
export ZSH_HIGHLIGHT_STYLES[comment]='fg=242'

# Delta
compdef _gnu_generic delta

# Terraform
if which terraform > /dev/null 2>&1
then
  command -v complete >/dev/null || {
    autoload -U +X bashcompinit && bashcompinit -u
  }

  alias tf=terraform
  complete -o nospace -C ~/.local/bin/terraform terraform
fi

# Kubernetes
if which kubectl > /dev/null 2>&1
then
  alias k=kubectl
  source <(kubectl completion zsh)
fi

# Helm
if which helm > /dev/null 2>&1
then
  source <(helm completion zsh)
fi

# Flux
if which flux > /dev/null 2>&1
then
  source <(flux completion zsh)
fi

# Just
if which just > /dev/null 2>&1
then
  source <(just --completions zsh)
fi

