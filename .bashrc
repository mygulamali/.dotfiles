# source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# if not running interactively, don't do anything
[ -z "$PS1" ] && return

# control history
shopt -s histappend
HISTCONTROL=ignoredups:ignorespace
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# ruby setup
export RAILS_ENV=development
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# number of threads to use for Intel MKL
export MKL_NUM_THREADS=1

# Setting PATH for Python 2.7
# The orginal version is saved in .bash_profile.pysave
#PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
#export PATH

# Setting PATH for EPD-7.2-2
# The orginal version is saved in .bash_profile.pysave
#PATH="/Library/Frameworks/EPD64.framework/Versions/Current/bin:${PATH}"
#export PATH

# pythonbrew (https://github.com/utahta/pythonbrew)
[[ -s "$HOME/.pythonbrew/etc/bashrc" ]] && source "$HOME/.pythonbrew/etc/bashrc"
pythonbrew switch 2.7.2

# virtualenvwrapper
source "$HOME/.pythonbrew/pythons/Python-2.7.2/bin/virtualenvwrapper.sh"

# minimise pip downloads
export PIP_DOWNLOAD_CACHE="$HOME/.pip/cache"
if [ ! -d $PIP_DOWNLOAD_CACHE ]; then
    mkdir -p $PIP_DOWNLOAD_CACHE
fi

# function to get current branch if PWD is a git repo
function parse_git_branch {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "("${ref#refs/heads/}")"
}

# command line prompt
if [ $TERM == 'xterm-256color' ]; then
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

# aliases
alias ls='ls -Gp'
alias ll='ls -alh'
alias psa='ps aux'
alias grep='grep -nr'
alias emacs='/usr/local/bin/emacs-23.4'
alias gita='git add'
alias gitb='git branch'
alias gits='git status'
