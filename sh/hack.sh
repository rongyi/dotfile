#!/bin/bash
# open tmux with six window automatically

# steal from https://gist.github.com/earthgecko/3089509
sn=`cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 32 | head -n 1`
tmux new-session -s "$sn" -d

for i in {1..6}; do
    tmux new-window -t "$sn:$i" -n "var$i"
done

tmux select-window -t "$sn:1"
tmux -2 attach-session -t "$sn"
