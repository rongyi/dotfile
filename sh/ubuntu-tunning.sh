#!/bin/bash

# disable super
dconf write /org/compiz/profiles/unity/plugins/unityshell/show-launcher '""'

# change alt-tab to my habit
sudo apt-get install -y compiz-plugins compiz-plugins-extra compizconfig-settings-manager
