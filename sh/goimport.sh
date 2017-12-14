#!/bin/bash
# I can not rememeber all the package import in my head
# `find' help me to find the package import path

if [[ $# -eq 0 ]]
then
    find $GOPATH/src/github.com -maxdepth 2 -type d
else
    find $GOPATH/src/github.com -maxdepth 2 -type d|grep $1|sed -e 's#.*/github#github#'
fi
