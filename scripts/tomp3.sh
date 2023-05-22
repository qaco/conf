#! /bin/bash

if [[ $# -eq 0 ]] ; then
    echo "usage: $(basename $0) DIRNAME" 1>&2
    exit 1
fi

find "$1" -type f -name '*.flac' | \
    parallel --bar \
	     --jobs "$(nproc --all)" \
	     '(ffmpeg -i {} -qscale:a 0 {.}.mp3 2> /dev/null && rm {.}.flac)'
