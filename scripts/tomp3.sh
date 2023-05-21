#! /bin/bash

if [[ $# -eq 0 ]] ; then
    echo "$(basename -- $0): argument needed (mp3 directory)." 1>&2
    exit 1
fi

parallel --bar --jobs 3 '(ffmpeg -i {} -qscale:a 0 {.}.mp3 2> /dev/null && rm {.}.flac)' ::: "$1"/*.flac
