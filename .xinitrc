#!/bin/bash

# Load xresources
[[ -f ~/.Xresources ]] && xrdb -merge -I"$HOME" ~/.Xresources

# Per host xinitrc
XINITRC_HOST_SCRIPT_DIR=~/dotfiles/per-host/$HOSTNAME/xinitrc
if [ -d "$XINITRC_HOST_SCRIPT_DIR" ]; then
    for SCRIPT in $XINITRC_HOST_SCRIPT_DIR/*; do
        $SCRIPT
    done;
fi;

# Launch i3
i3
