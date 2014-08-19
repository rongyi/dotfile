#!/bin/zsh

for repo in ~/.vim/bundle/*
do
    cd $repo
    git remote -v 2> /dev/null | grep fetch|sed -e 's/origin\t//' -e 's/ (fetch)$//'
    cd ../
done
