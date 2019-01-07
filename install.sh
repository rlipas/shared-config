#!/bin/bash
# Run this script on your home directory!

DIR=`dirname $0`

cd

# .bashrc
ln -sf $DIR/bash/bashrc .bashrc

# .gitconfig
ln -sf $DIR/git/gitconfig .gitconfig

# .pystartup
ln -sf $DIR/python/pystartup .pystartup

# .inputrc
ln -sf $DIR/readline/inputrc .inputrc

# .vim + .vimrc
ln -sf $DIR/vim/ .vim
ln -sf $DIR/vim/vimrc .vimrc
vim -c PlugInstall -c qa

# .dir_colors
ln -sf $DIR/dircolors-zenburn/dircolors .dircolors

# xmonad.hs + xmobarrc
mkdir -p .xmonad
ln -sf $DIR/xmonad/xmonad.hs .xmonad/xmonad.hs
mkdir -p .config/xmobar
ln -sf $DIR/xmobar/xmobarrc .config/xmobar/xmobarrc

# kitty.conf
mkdir -p .config/kitty
ln -fs $DIR/kitty/kitty.conf .config/kitty/kitty.conf
