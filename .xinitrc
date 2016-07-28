#!/bin/bash

# Load xresources
[[ -f ~/.Xresources ]] && xrdb -merge -I"$HOME" ~/.Xresources

# Launch i3
i3
