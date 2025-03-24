#! /bin/bash

if [[ -x /usr/bin/dircolors ]]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --exclude-dir venv -I --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias git='LANG=en_GB git'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

if tty | grep -q 'tty'; then
    alias emacsclient="TERM=xterm-256color emacsclient -nw -c --alternate-editor= "
    alias emacs="TERM=xterm-256color emacs -nw"
else
    alias emacsclient="emacsclient -nw -c --alternate-editor= "
    alias emacs="emacs -nw"
fi

alias mc="TERM=xterm-256color mc"
alias neomutt="TERM=xterm-256color neomutt"

alias mvlc='nvlc -Z --no-metadata-network-access'
alias cvlc='cvlc -Z --no-metadata-network-access'
alias glg='git log --graph --oneline --decorate --color=always | less -R'
alias pdflatex='pdflatex -halt-on-error'
alias neofetch='neofetch --gtk3 off --disable memory uptime'
alias emacs-agenda='emacs -f org-agenda-list'
alias emacs-todo='emacs -f org-todo-list'
alias emacs-journal='emacs -f my-org-journal-new-entry'
alias emacs-term='emacs -f multi-vterm'
alias emacs-base='emacs -q --load=~/conf/.emacs.d/standalone.el'

alias FileCheck=FileCHeck-14
