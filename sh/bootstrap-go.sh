#!/bin/bash

#editor related
# no longer maintained
#go get -u -v github.com/nsf/gocode
go get -u -v github.com/mdempsky/gocode
go get -u -v github.com/rogpeppe/godef
go get -u -v golang.org/x/tools/cmd/guru
go get -u github.com/golang/lint/golint
go get -v github.com/zmb3/gogetdoc
# linter, make editor too slow
# go get -u gopkg.in/alecthomas/gometalinter.v1
# gometalinter.v1 --install --update

# profile
go get -v github.com/uber/go-torch
# request tool
go get -v github.com/rakyll/hey
