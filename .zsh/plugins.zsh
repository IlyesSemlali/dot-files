if [ -f $HOME/.local/share/zplug/init.zsh ]; then

    source ~/.local/share/zplug/init.zsh

    zplug "b4b4r07/enhancd", use:init.sh
    zplug "ianthehenry/sd"

    zplug load
fi

