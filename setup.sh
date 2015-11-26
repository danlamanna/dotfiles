#!/bin/bash

# htop
mkdir -p ~/.config/htop
ln -s ~/dotfiles/.config/htop/htoprc ~/.config/htop/htoprc

# emacs
mkdir -p ~/.config/systemd/user
ln -s ~/dotfiles/.config/systemd/user/emacs.service ~/.config/systemd/user/emacs.service

# mpv
mkdir -p ~/.config/mpv
ln -s ~/dotfiles/.config/mpv/mpv.conf ~/.config/mpv/mpv.conf

if ! systemctl is-enabled --user emacs; then
    systemctl enable --user emacs
fi

# openbox/tint2
ln -s ~/dotfiles/.config/openbox/ ~/.config/
ln -s ~/dotfiles/.config/tint2/ ~/.config/

# rest
ln -s ~/dotfiles/.gitignore_global   ~/.gitignore_global
ln -s ~/dotfiles/.gitconfig          ~/.gitconfig
ln -s ~/dotfiles/.tmux.conf          ~/.tmux.conf
ln -s ~/dotfiles/.Xmodmap            ~/.Xmodmap
ln -s ~/dotfiles/.zshrc              ~/.zshrc
