source ~/.local/share/homebrew/opt/zplug/init.zsh

zplug "b4b4r07/enhancd", use:init.sh


###########################
# Nice Feauture of zplug: #
###########################

# # Load if "if" tag returns true
# zplug "lib/clipboard", from:oh-my-zsh, if:"[[ $OSTYPE == *darwin* ]]"

# # Run a command after a plugin is installed/updated
# # Provided, it requires to set the variable like the following:
# # ZPLUG_SUDO_PASSWORD="********"
# zplug "jhawthorn/fzy", \
#     as:command, \
#     rename-to:fzy, \
#     hook-build:"make && sudo make install"


# # Can manage local plugins
# zplug "~/.zsh", from:local

#####################################################################
# Install plugins if there are plugins that have not been installed #
#####################################################################

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load
