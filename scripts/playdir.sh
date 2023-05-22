#! /bin/bash

if [[ $# -ne 1 ]] ; then
    echo "usage: $(basename $0) MUSICDIR" 1>&2
    exit 1
fi

tom3u.sh "$(realpath "$1")" /tmp/playlist.m3u
nvlc -Z --no-metadata-network-access /tmp/playlist.m3u
