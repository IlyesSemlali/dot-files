if [ -f $HOME/.local/share/homebrew/opt/zplug/init.zsh ]; then

    source ~/.local/share/homebrew/opt/zplug/init.zsh

    zplug "b4b4r07/enhancd", use:init.sh

    zplug load
fi
