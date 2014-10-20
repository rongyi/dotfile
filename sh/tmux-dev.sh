#!/bin/zsh
tmux new-session -d
tmux split-window -h
tmux new-window "ssh rydev"
tmux new-window
tmux new-window
tmux new-window "cd /home/ry/message/proto "
tmux -2 attach-session -d
