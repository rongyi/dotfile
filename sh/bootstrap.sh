#!/bin/bash

# packages for dev
sudo apt-get install build-essential emacs git gnome-terminal libatk1.0-dev libbonoboui2-dev libcairo2-dev libcurl3 libcurl4-openssl-dev libexpat1-dev libexpat1-dev libgif-dev libgnome2-dev libgnomeui-dev libgnutls-dev libgtk2.0-dev libjpeg8-dev libncurses5-dev libpcre3-dev libpng++-dev libssh-dev libtiff5-dev libx11-dev libxaw7-dev libxpm-dev libxt-dev openssh-server python-dev ruby-dev xaw3dg-dev zsh exuberant-ctags libtool automake python-pip cmake libevent-dev

# build vim
cd ~
git clone https://github.com/vim/vim.git
cd vim
./configure --with-features=huge \
            --enable-multibyte \
            --enable-rubyinterp \
            --enable-pythoninterp \
            --with-python-config-dir=/usr/lib/python2.7/config \
            --enable-perlinterp \
            --enable-luainterp \
            --enable-gui=gtk2 --enable-cscope --prefix=/usr
make VIMRUNTIMEDIR=/usr/share/vim/vim74
sudo make install

# silver searcher
git clone https://github.com/ggreer/the_silver_searcher.git

# python lib
pip install yapf
