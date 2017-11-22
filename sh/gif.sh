#!/bin/bash

XWININFO=$(xwininfo)
X=$(echo "$XWININFO" | awk -F: '/Absolute upper-left X/{print $2}')
Y=$(echo "$XWININFO" | awk -F: '/Absolute upper-left Y/{print $2}')
W=$(echo "$XWININFO" | awk -F: '/Width/{print $2}')
H=$(echo "$XWININFO" | awk -F: '/Height/{print $2}')

echo $X
echo $Y
echo $W
echo $H

byzanz-record -c --verbose --delay=0 --duration=15 --x="$X" --y="$Y" --width="$W" --height="$H" "/tmp/out.gif"
