# ~/.zshrc: executed by zsh for interactive shells

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# History configuration
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=2000
setopt APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS

# Make less more friendly for non-text input files
[[ -x /usr/bin/lesspipe ]] && eval "$(SHELL=/bin/sh lesspipe)"

# Git branch parsing function
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/' -e 's/(\(.*\))/\1/' -e 's/\(.*\)/ (\1)/'
}

# Prompt configuration
autoload -U colors && colors
setopt PROMPT_SUBST

if [[ $UID -eq 0 ]]; then
    PROMPT="%{$fg[blue]%}%1~%{$reset_color%} # "
else
    PROMPT="%B%{$fg[blue]%}%1~%{$reset_color%}%{$fg[green]%}\$(parse_git_branch)%{$reset_color%}%B%{$fg[blue]%} $%{$reset_color%} "
fi
PROMPT2="%{$fg[white]%}> %{$reset_color%}"

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
export COLORTERM=truecolor
export TERM=xterm-direct
export VISUAL="emacs -nw"
export EDITOR="$VISUAL"

[[ -f ~/.bash_aliases ]] && source ~/.bash_aliases
[[ -f ~/.paths ]] && source ~/.paths

# Enable completion system
autoload -Uz compinit && compinit
