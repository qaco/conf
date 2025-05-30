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
    # PS1=" ${BOLD}${BLUE}\W${REG}${GREEN}\$(parse_git_branch)${BOLD}${BLUE} \$${WHITE} \[\e]2;\W\a\]"
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

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
export COLORTERM=truecolor
export TERM=xterm-direct
export VISUAL="emacs -nw"
export EDITOR="$VISUAL"

[[ -f ~/.bash_aliases ]] && source ~/.bash_aliases
[[ -f ~/.paths ]] && source ~/.paths
