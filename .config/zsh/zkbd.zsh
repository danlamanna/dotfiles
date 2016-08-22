bindkey -e

source "$HOME/.config/zsh/rxvt-unicode-256color"

[ -n "${key[Delete]}" ]  && bindkey "${key[Delete]}"  delete-char
