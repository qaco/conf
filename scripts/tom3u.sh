#! /bin/bash

if [[ $# -ne 2 ]] ; then
    echo "usage: $(basename $0) MUSICDIR M3UFILE" 1>&2
    exit 1
fi

numfiles=$(find "$1" -name "*.flac" | wc -l)
i=0

echo "#EXTM3U" > $2

shopt -s globstar
for f in "$1"/**/*.flac; do
    track=$(ffprobe "$f" |& grep -i -m 1 "track" | cut -d ':' -f 2 | sed 's/^ //g')
    title=$(ffprobe "$f" |& grep -i -m 1 "title" || echo ' : Unknown' | cut -d ':' -f 2 | sed 's/^ //g')
    artist=$(ffprobe "$f" |& grep -i -m 1 "artist" || echo ' : Unknown' | cut -d ':' -f 2 | sed 's/^ //g')
    echo "#EXTINF:$track, $artist - $title" >> $2
    dname1="$(realpath "$f")"
    echo "$dname1" >> $2
    ((i=i+1))
    vramsteg --label "$i/$numfiles" --min 0 --max $numfiles --current $i
done
vramsteg --remove
