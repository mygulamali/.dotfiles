# source global definitions
[[ -f /etc/bashrc ]] && . /etc/bashrc

# if not running interactively, don't do anything
[[ -z "$PS1" ]] && return

# control history
shopt -s histappend
HISTCONTROL=ignoredups:ignorespace
HISTSIZE=1000
HISTFILESIZE=2000

# number of threads to use for Intel MKL
export MKL_NUM_THREADS=1

# aliases
unalias -a
alias ls='ls -Gp'
alias ll='ls -aFhl'
alias psa='ps aux'
alias grep='grep -n --color=auto'
alias pingg='ping -c 3 www.google.com'

# function to get current branch if PWD is a git repo
function parse_git_branch {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || return
    echo "("${ref#refs/heads/}")"
}

# command line prompt
if [ $(tput colors) -ge 8 ]; then
    BLACK="\[\033[0;30m\]"
    RED="\[\033[0;31m\]"
    GREEN="\[\033[0;32m\]"
    YELLOW="\[\033[0;33m\]"
    BLUE="\[\033[0;34m\]"
    MAGENTA="\[\033[0;35m\]"
    CYAN="\[\033[0;36m\]"
    WHITE="\[\033[0;37m\]"
    NORMAL="\[\033[0;0m\]"
    export PS1="$RED\u@\h $BLUE\W $YELLOW\$(parse_git_branch)$NORMAL\n\\$ "
else
    export PS1="\u@\h \W $(parse_git_branch)\n\\$ "
fi

# pythonbrew (https://github.com/utahta/pythonbrew)
[[ -s "$HOME/.pythonbrew/etc/bashrc" ]] && source "$HOME/.pythonbrew/etc/bashrc"
pythonbrew switch 2.7.2

# virtualenvwrapper
source "$HOME/.pythonbrew/pythons/Python-2.7.2/bin/virtualenvwrapper.sh"

# minimise pip downloads
export PIP_DOWNLOAD_CACHE="$HOME/.pip/cache"
[[ ! -d $PIP_DOWNLOAD_CACHE ]] && mkdir -p $PIP_DOWNLOAD_CACHE

# rvm
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# rails
export RAILS_ENV=development

# add node.js build
export NODE_HOME=${HOME}/Projects/node
if [ -d ${NODE_HOME} ]; then
    export PATH=${PATH}:${NODE_HOME}/bin
    export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${NODE_HOME}/lib
fi

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# the one true editor
export EDITOR=emacs
if [ -x /usr/local/bin/emacs ]; then
    alias emacs='/usr/local/bin/emacs'
fi

# add personal bin directory (this should always be at the end)
if [ -d ${HOME}/bin ]; then
    export PATH=${HOME}/bin:${PATH}
fi
