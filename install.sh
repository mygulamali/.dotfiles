#!/bin/sh

if [ `dirname $0` != '.' ]; then
  DOTFILES_DIR=$(find . -name .dotfiles -print -quit 2> /dev/null)
  cd $DOTFILES_DIR
fi

FILES=$(ls -1p | egrep ^_.* | egrep -v /$)

for FILE in $FILES; do
  DOTFILE=$HOME/.${FILE:1}
  if [ -s $DOTFILE ]; then
    if [ -L $DOTFILE ]; then
      # $DOTFILE exists as a symlink
      rm $DOTFILE
    else
      # $DOTFILE exists as a real file
      mv $DOTFILE $PWD/defaults/_${DOTFILE:1}
    fi
  fi
  ln -s $PWD/$FILE $DOTFILE
done
