#!/bin/bash

# command prompt
if [ $(tput colors) -ge 8 ]; then
  export PS1="$GREEN[\t] $RED\u@\h $BLUE\W $YELLOW\$(parse_git_branch)$NORMAL\n\\$ "
else
  export PS1="[\t] \u@\h \W $(parse_git_branch)\n\\$ "
fi

# asdf (https://asdf-vm.com/)
if type brew &>/dev/null; then
  source $(brew --prefix asdf)/libexec/asdf.sh
elif [ -d "$HOME/.asdf" ]; then
  source "$HOME/.asdf/asdf.sh"
  source "$HOME/.asdf/completions/asdf.bash"
fi
