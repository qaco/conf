# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend     

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

#aliases

alias git='LANG=en_GB git'

#colored prompt

parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/' -e 's/(\(.*\))/\1/' -e 's/\(.*\)/ (\1)/'
}

BOLD="\[\033[01m\]"
REG="\[\033[0m\]"
RED="\[\033[31m\]"
YELLOW="\[\033[33m\]"
GREEN="\[\033[32m\]"
BLUE="\[\033[34m\]"
WHITE="\[\033[00m\]"

if [ $(id -u) -eq 0 ];
then
    PS1=" ${BLUE}\W #${WHITE} "
else
    PS1=" ${BOLD}${BLUE}\W${REG}${GREEN}\$(parse_git_branch)${BOLD}${BLUE} \$${WHITE} "
fi
PS2=" ${WHITE}> "

# colored manpages
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;31m'
export LESS_TERMCAP_mb=$'\E[01;34m'
export LESS_TERMCAP_md=$'\E[01;34m'
# export LESS_TERMCAP_so=$'\E[01;44;33m'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

if [ -x /usr/bin/mint-fortune ]; then
     /usr/bin/mint-fortune
fi

export COLORTERM=truecolor
export TERM=xterm-direct

export VISUAL="emacs -nw"
export EDITOR="$VISUAL"
if tty | grep -q 'tty'; then
    alias emacsclient="TERM=xterm-256color emacsclient -nw -c --alternate-editor= "
    alias emacs="TERM=xterm-256color emacsclient -nw -c --alternate-editor= "
else
    alias emacsclient="emacsclient -nw -c --alternate-editor= "
    alias emacs="emacsclient -nw -c --alternate-editor= "
fi

alias mc="TERM=xterm-256color mc"
alias neomutt="TERM=xterm-256color neomutt"

export PATH=$PATH:/sbin/
export PATH=$PATH:~/src/naivecalendar/src/
export PATH=$PATH:~/conf/scripts/
export PATH=$PATH:~/src/tensorflow-2.12.0/bazel-out/k8-opt/bin/tensorflow/compiler/mlir/
export PATH=$PATH:~/bin/
export PATH=$PATH:~/bin/llvm/bin/

alias naivecalendar="naivecalendar.sh -t onedark"
alias mvlc='nvlc -Z --no-metadata-network-access'
alias cvlc='cvlc -Z --no-metadata-network-access'
alias nterm='mate-terminal --window'

alias pdflatex='pdflatex -halt-on-error'
# Reminder:

# Rip
# abcde -o flac
