if [ -f $HOME/.local/share/zplug/init.zsh ]; then

    source ~/.local/share/zplug/init.zsh

    zplug "ianthehenry/sd"
    zplug load
fi

if [ -f $HOME/.local/share/fzf-tab-completion/zsh/fzf-zsh-completion.sh ]; then
  source $HOME/.local/share/fzf-tab-completion/zsh/fzf-zsh-completion.sh
fi

