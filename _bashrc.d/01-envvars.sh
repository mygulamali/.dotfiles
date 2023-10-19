#!/bin/bash

# control history
HISTCONTROL=ignoredups:ignorespace
HISTSIZE=1000
HISTFILESIZE=2000

# the one true editor
export EDITOR=emacs

# locale settings
export LC_ALL=$LANG

# colors
if [ $(tput colors) -ge 8 ]; then
  export BLACK="\[\033[0;30m\]"
  export RED="\[\033[0;31m\]"
  export GREEN="\[\033[0;32m\]"
  export YELLOW="\[\033[0;33m\]"
  export BLUE="\[\033[0;34m\]"
  export MAGENTA="\[\033[0;35m\]"
  export CYAN="\[\033[0;36m\]"
  export WHITE="\[\033[0;37m\]"
  export NORMAL="\[\033[0;0m\]"
fi
