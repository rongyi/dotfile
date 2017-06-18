#!/bin/bash

# change default shell to zsh
chsh -s $(which zsh)

# copy content from this link
https://www.topbug.net/blog/2013/04/14/install-and-use-gnu-command-line-tools-in-mac-os-x/

#workaround for ycmd restart
https://github.com/Valloric/ycmd/issues/126


# install emacs for mac, other way emacs kind of slow, this is the fastest I found
# https://github.com/howardabrams/dot-files/blob/master/emacs.org
brew cask install xquartz

brew edit emacs

# And add the following section

patch do
  url "https://github.com/minimal/emacs/commit/812dd5119645a09bc025a9dddedad9474d12ecb6.diff"
end

brew install emacs --HEAD --use-git-head --with-cocoa --with-gnutls --with-librsvg --with-ns --with-imagemagick

brew linkapps emacs
