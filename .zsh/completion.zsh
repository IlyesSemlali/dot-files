# # TODO: Add comment for each completion style and tidy this mess up
#
# zstyle ':urlglobber' url-other-schema
zstyle ':completion:*:*:*:*:*' menu select

# # case insensitive (all), partial-word and substring completion
# zstyle ':completion:*' matcher-list 'r:|=*' 'l:|=* r:|=*'
#
# # Complete . and .. special directories
# zstyle ':completion:*' special-dirs true

# # disable named-directories autocompletion
# zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
#
# # TODO: Use caching so that commands like apt and dpkg complete are useable
# # zstyle ':completion:*' use-cache yes
# # zstyle ':completion:*' cache-path $ZSH_CACHE_DIR
#
# # TODO
# # Don't complete uninteresting users
# zstyle ':completion:*:*:*:users' ignored-patterns \
#         adm amanda apache at avahi avahi-autoipd beaglidx bin cacti canna \
#         clamav daemon dbus distcache dnsmasq dovecot fax ftp games gdm \
#         gkrellmd gopher hacluster haldaemon halt hsqldb ident junkbust kdm \
#         ldap lp mail mailman mailnull man messagebus mldonkey mysql nagios \
#         named netdump news nfsnobody nobody nscd ntp nut nx obsrun openvpn \
#         operator pcap polkitd postfix postgres privoxy pulse pvm quagga radvd \
#         rpc rpcuser rpm rtkit scard shutdown squid sshd statd svn sync tftp \
#         usbmux uucp vcsa wwwrun xfs '_*'
#
# # ... unless we really want to.
# zstyle '*' single-ignored show
#
# # TODO: understand how compdef works to implement it later
# # function d () {
# #   if [[ -n $1 ]]; then
# #     dirs "$@"
# #   else
# #     dirs -v | head -n 10
# #   fi
# # }
# # compdef _dirs d

# Match partial words, case insensitively
zstyle ':completion:*' matcher-list \
    'm:{[:lower:]}={[:upper:]}' \
    '+r:|[._-]=* r:|=*' \
    '+l:|=*'

# Zsh to use the same colors as ls
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}  # zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

