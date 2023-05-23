#! /bin/bash

if [[ $# -ne 2 ]] ; then
    echo "usage: $(basename $0) MUSICDIR M3UFILE" 1>&2
    exit 1
fi

# Header of the playlist
echo "#EXTM3U" > $2

dname0="$(realpath "$1")"
numfiles=$(find "$1" -regex ".*\.\(mp3\|flac\)" | wc -l)
i=0
START=$(vramsteg --now)
# For each flac or mp3 file (recursively)
shopt -s globstar
for f in "$dname0"/**/*.{flac,mp3}; do
    # Get metadata
    rprt=$(ffprobe "$f" |& cat)
    # Parse metadata
    track=$(echo "$rprt" | grep -i -m 1 "track" | cut -d ':' -f 2 | sed 's/^ //g')
    title=$(echo "$rprt" | grep -i -m 1 "title" || echo ' : Unknown' | cut -d ':' -f 2)
    artist=$(echo "$rprt" | grep -i -m 1 "artist" || echo ' : Unknown' | cut -d ':' -f 2)
    # Write metadata of the playlist
    # (test because there is a bug)
    if [ -f "$f" ]
    then
	   echo "#EXTINF:$track, $artist - $title" | tr -s " " >> $2
	   echo "$f" >> $2
    fi
    # Update counter
    ((i=i+1))
    # Update progress bar
    lab="$(printf '%10s ' "$i/$numfiles")"
    vramsteg --label "$lab" vramsteg --start=$START --estimate --min 0 --max $numfiles --current $i
done
vramsteg --remove
