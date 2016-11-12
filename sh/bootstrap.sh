#!/bin/bash

# 2016年10月01日 星期六 10时04分14秒, update some software if needed

# packages for dev
sudo apt-get install -y build-essential emacs git gnome-terminal libatk1.0-dev libbonoboui2-dev libcairo2-dev libcurl3 libcurl4-openssl-dev libexpat1-dev libexpat1-dev libgif-dev libgnome2-dev libgnomeui-dev libgnutls-dev libgtk2.0-dev libjpeg8-dev libncurses5-dev libpcre3-dev libpng++-dev libssh-dev libtiff5-dev libx11-dev libxaw7-dev libxpm-dev libxt-dev openssh-server python-dev ruby-dev xaw3dg-dev zsh exuberant-ctags libtool automake python-pip cmake libevent-dev vim

# silver searcher
git clone https://github.com/ggreer/the_silver_searcher.git
# oh my zsh
git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh

# Emacs is now my favorite, latest vim is not needed, or I use Vim in Emacs :)
# cd ~
# git clone https://github.com/vim/vim.git
# cd vim
# ./configure --with-features=huge \
#             --enable-multibyte \
#             --enable-rubyinterp \
#             --enable-pythoninterp \
#             --with-python-config-dir=/usr/lib/python2.7/config \
#             --enable-perlinterp \
#             --enable-luainterp \
#             --enable-gui=gtk2 --enable-cscope --prefix=/usr
# make VIMRUNTIMEDIR=/usr/share/vim/vim74
# sudo make install


# python lib
pip install yapf flake8 jedi mycli

# gnome terminal theme
# dracular: https://github.com/Mayccoll/Gogh/blob/master/content/themes.md
wget -O xt  http://git.io/v3D8e && chmod +x xt && ./xt && rm xt

# download emacs 25 from https://www.gnu.org/software/emacs/download.html
echo "Downloading the latest emacs"
wget -c http://ftp.twaren.net/Unix/GNU/gnu/emacs/emacs-25.1.tar.gz

echo "Downloading gtags"
wget -c http://tamacom.com/global/global-6.5.5.tar.gz

# install c++doc e.g. libstdc++-4.9-doc

echo "You can build thest source code now, your highness"
