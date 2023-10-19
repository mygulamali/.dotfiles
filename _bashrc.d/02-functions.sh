#!/bin/bash

# function to pull main and then go back to your current branch
function pullmain {
  current=$(git symbolic-ref HEAD 2> /dev/null) || return
  git checkout main && git pull && git checkout ${current#refs/heads/}
}

# function to get current branch if PWD is a git repo
function parse_git_branch {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "("${ref#refs/heads/}")"
}

# function to open Github project page for current repo (hat tip to @sardaukar)
function gh {
  url=$(git config -l | grep --color=never "remote.origin.url" | sed -En "s/remote.origin.url=git(@|:\/\/)github.com(:|\/)(.+)\/(.+).git/https:\/\/github.com\/\3\/\4/p")
  if echo "opening ${url}" | grep http; then
    open ${url}
  else
    echo "no git repo found!"
    return 1
  fi
}

# function to flush DNS cache in Mac OS X (El Capitan)
function flush_dns {
  if [ $(uname -s) == 'Darwin' ]; then
    sudo dscacheutil -flushcache
    sudo killall -HUP mDNSResponder
  fi
}
