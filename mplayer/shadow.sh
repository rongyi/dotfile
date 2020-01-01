#!/bin/zsh
#A simple mplayer wrapper to shadow the Chinese subtitle when I can not switch
#to English subtitle

TIME_DURATION="00:00:00,000 --> 03:00:15,000"
SHADOW_ELM="█████████████████████████████████████████████"
# I don't want to copy a src file to a specific place, instead, I create it
# everytime when I execute this script.
SHADOW_FILE="/tmp/mplayer-shadow.srt"

# generate subtitle file
echo "1" > $SHADOW_FILE
echo "$TIME_DURATION" >> $SHADOW_FILE
echo "$SHADOW_ELM" >> $SHADOW_FILE

if [[ $# -lt 1 ]]
then
    echo "You need to specify the movie you want to play"
    exit -1
fi

# you can use 'r' and 't' to change the position to shadow exactly the subtitle
# you want.
mplayer -sub $SHADOW_FILE -subpos 95 $1



