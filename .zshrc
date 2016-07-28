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
