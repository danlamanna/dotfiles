#!/bin/bash

# Load xresources
[[ -f ~/.Xresources ]] && xrdb -merge -I"$HOME" ~/.Xresources

# Per host xinitrc
XINITRC_HOST_SCRIPT_DIR=~/dotfiles/.host-specific/$HOSTNAME/xinitrc
if [ -d "$XINITRC_HOST_SCRIPT_DIR" ]; then
    for SCRIPT in $XINITRC_HOST_SCRIPT_DIR/*; do
        $SCRIPT
    done;
fi;

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

dunst &

# Launch i3
i3
