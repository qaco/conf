#! /bin/bash

# Args
# $1: media source
# $2: preffix emoji
# $3: title length max (characters)
# $4: artist length max (characters)

play=$(playerctl status -p $1 | grep -c "Playing")
pause=$(playerctl status -p $1 | grep -c "Paused")
if [ $play -eq 1 ] || [ $pause -eq 1 ]
then
    # Print preffix
    if [ $play -eq 1 ]
    then
        preffix=$(echo "$2▶️ ")
    elif [ $pause -eq 1 ]
    then
        preffix=$(echo "$2⏸️ ")
    fi
    # Fetch metada
    title=$(playerctl metadata --format '{{ title }}' -p $1 | sed 's/\"/\\"/g' | sed "s/\(.\{$3\}\).*/\1.../")
    artist=$(playerctl metadata --format '{{ artist }}' -p $1 | sed 's/\"/\\"/g' | sed "s/\(.\{$4\}\).*/\1.../")
    # Eventually print
    echo "[$preffix $title // $artist]"
fi
