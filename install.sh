#!/bin/bash
# Run this script on your home directory!

DIR=`dirname $0`

cd

# .bashrc
ln -srf $DIR/bash/bashrc .bashrc

# .gitconfig
ln -srf $DIR/git/gitconfig .gitconfig

# .pystartup
ln -srf $DIR/python/pystartup .pystartup

# .inputrc
ln -srf $DIR/readline/inputrc .inputrc

# .vim + .vimrc
ln -srf $DIR/vim/ .vim
ln -srf $DIR/vim/vimrc .vimrc
vim -c PlugInstall -c qa

# .dir_colors
ln -srf $DIR/dircolors-zenburn/dircolors .dircolors

# xmonad.hs + xmobarrc
mkdir -p .config/xmonad
ln -srf $DIR/xmonad/xmonad.hs .config/xmonad/xmonad.hs
mkdir -p .config/xmobar
ln -srf $DIR/xmobar/xmobarrc .config/xmobar/xmobarrc

# kitty.conf
mkdir -p .config/kitty
ln -srf $DIR/kitty/kitty.conf .config/kitty/kitty.conf
