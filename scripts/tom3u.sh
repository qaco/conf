#! /bin/bash

if [[ $# -ne 2 ]] ; then
    echo "usage: $(basename $0) MUSICDIR M3UFILE" 1>&2
    exit 1
fi

find "$1" -name *.flac | sort -R > "$2"
