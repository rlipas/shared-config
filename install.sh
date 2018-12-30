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

#.inputrc
ln -sf $DIR/readline/inputrc .inputrc

#.vim + .vimrc
ln -sf $DIR/vim/ .vim
ln -sf $DIR/vim/vimrc .vimrc
vim -c PlugInstall -c qa

#.tmux.conf
ln -sf $DIR/tmux/tmux.conf .tmux.conf

#.dir_colors
ln -sf $DIR/dircolors-solarized/dircolors.256dark .dircolors
