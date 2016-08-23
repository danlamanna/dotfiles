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
alias rsynca="rsync -avhP --append"
alias gruntl="./node_modules/.bin/grunt"
alias gruntld="gruntl --debug-js"
alias splitsane="split --numeric-suffixes --additional-suffix=.part"

setopt autocd
setopt autopushd


HISTFILE=$HOME/.local/share/zsh/history
HISTSIZE=999999999
SAVEHIST=$HISTSIZE
setopt appendhistory
setopt extendedhistory
setopt histignoredups
setopt histignorespace

for filename in "$HOME"/.config/zsh/*.zsh; do
    source "$filename";
done

# NVM
if [ -d "$HOME/.nvm" ]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
fi

# Virtualenvwrapper
export WORKON_HOME=~/.virtualenvs
mkdir -p $WORKON_HOME
source /usr/bin/virtualenvwrapper.sh

# Prepend ~/bin to PATH
# This was mostly done to correct for poorly behaving packages (vagrant on Fedora)
export PATH="$HOME/bin:$PATH"
