#!/bin/zsh

cp ./gitignore ~/.gitignore
#basic
git config --global user.name "rongyi"
git config --global user.email "yi.rong@yamutech.com"
git config --global color.ui true
#ignore file perm change
git config --global core.fileMode false
#put vim open with insert mode, if you like Emacs,
#you can use 'emacsclient -c' instead
git config --global core.editor vim -c 'startinsert'
git config --global http.proxy '192.168.1.99:8762'
#alias
git config --global alias.ls 'status -sb'
git config --global alias.cm commit
git config --global alias.co checkout
git config --global alias.lg "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative"

#set gui diff tool to meld
git config --global diff.tool meld
git config --global diff.guitool meld
git config --global difftool.prompt false

# With this, "git pull --rebase" is the default form of pull FOR NEWLY CREATED
# BRANCHES; for branches created before this config option is set pull.rebase
# true handles that
git config --global branch.autosetuprebase always

# "git pull --rebase" is now the default for pull no matter what
git config --global pull.rebase true

# This makes sure that push pushes only the current branch, and pushes it to the
# same branch pull would pull from
git config --global push.default upstream

# This converts CRLF endings to LF endings on Mac & Lin and also keeps them in
# the repo, but for Windows checkouts it converts LF to CRLF (and back to LF on
# commits)
git config --global core.autocrlf input

# add global ignore file
git config --global core.excludesfile ~/.gitignore

