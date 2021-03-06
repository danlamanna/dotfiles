# Handle dumb terminals well, for tramp
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

autoload -Uz compinit promptinit
compinit
promptinit

prompt walters

# Always rehash
zstyle ':completion:*' rehash true

autoload -U compinit && compinit

# Tell ls to stop adding quotes
export QUOTING_STYLE=literal

alias ls="ls --color"
alias l="ls -Alh"
alias lr="l -tr"
alias ll="ls -lh"
alias llr="ll -tr"
alias rsynca="rsync -avhP --append"
alias splitsane="split --numeric-suffixes --additional-suffix=.part"
alias mkvirtualenv3="mkvirtualenv --python=$(which python3)"
alias vup="vagrant up"
alias vprov="vagrant provision"
alias vdest="vagrant destroy -f"
alias vssh="vagrant ssh"
alias jn="jupyter notebook --no-browser"
alias jnb="jupyter notebook"

setopt autocd
setopt autopushd
setopt globdots

HISTFILE=$HOME/.local/share/zsh/history
HISTSIZE=999999999
SAVEHIST=$HISTSIZE
setopt appendhistory
setopt extendedhistory
setopt histignoredups
setopt histignorespace

# Stop trying to complete from my hosts file
zstyle ':completion:*' hosts off

for filename in "$HOME"/.config/zsh/*.zsh; do
    source "$filename";
done

# Virtualenvwrapper
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python2
export WORKON_HOME=~/.virtualenvs
mkdir -p $WORKON_HOME
source /usr/bin/virtualenvwrapper.sh

export PATH="$HOME/.local/bin:$HOME/bin:$PATH"
export EDITOR="/usr/bin/emacsclient -c"
