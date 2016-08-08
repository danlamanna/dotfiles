# Handle dumb terminals well, for tramp
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

autoload -Uz compinit promptinit
compinit
promptinit

prompt walters

# Always rehash
zstyle ':completion:*' rehash true

# Tell ls to stop adding quotes
export QUOTING_STYLE=literal

alias l="ls -Alh"
alias ll="ls -lh"

setopt autocd
setopt autopushd

setopt appendhistory
setopt extendedhistory
setopt histignoredups
setopt histignorespace

# NVM
if [ -d "$HOME/.nvm" ]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
fi

# Virtualenvwrapper
export WORKON_HOME=~/.virtualenvs
mkdir -p $WORKON_HOME
source /usr/bin/virtualenvwrapper.sh
