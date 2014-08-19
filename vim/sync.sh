#!/bin/zsh
#pathogen bundle save place

for repo in `ls ~/.vim/bundle`
do
    echo "update $repo ..."
    cd ~/.vim/bundle/$repo
    git pull
    git submodule update --init --recursive 2> /dev/null
    cd ../
done
