#!/bin/bash

function cmd_exists {
    $1 &> /dev/null && return 0
    return 1
}

function die {
    echo "$1" 1>&2
    exit 1
}

[ -z "$HOME" ] && die "Can't find your HOME (\$HOME is empty!?)"

# check for unzip
cmd_exists unzip || die "No such program"

# check for curl or wget, then get the zip
TARGET_ZIP="$HOME/dot-emacs-master.zip"

DL_PROG=""
if cmd_exists "curl -h"; then
    DL_PROG="curl -L -s -o $TARGET_ZIP"
elif cmd_exists "wget -h"; then
    DL_PROG="wget -q -O $TARGET_ZIP"
else
    die "You need curl or wget!"
fi

cd $HOME || die "Can't go \$HOME"

echo "Downloading dot-emacs from github..."
$DL_PROG https://github.com/ekenberg/dot-emacs/archive/master.zip

# (re)move existing .emacs.d
[ -h $HOME/.emacs.d ] && rm -f $HOME/.emacs.d
[ -e $HOME/.emacs.d ] && (echo "Backing up your current .emacs.d:"; rm -rf $HOME/.emacs.d.orig.bak; mv -v $HOME/.emacs.d $HOME/.emacs.d.orig.bak)
[ -h $HOME/.emacs ] && rm -f $HOME/.emacs
[ -e $HOME/.emacs ] && (echo "Backing up your current .emacs:"; rm -rf $HOME/.emacs.orig.bak; mv -v $HOME/.emacs $HOME/.emacs.orig.bak)

TARGET_DIR=$HOME/.emacs.d.dot-emacs

# (re)create $TARGET_DIR
rm -rf $TARGET_DIR
mkdir $TARGET_DIR || die

# unpack zip
cd $TARGET_DIR || die
unzip -q $TARGET_ZIP || die
mv dot-emacs-master/* ./ || die
rmdir dot-emacs-master
rm -f $TARGET_ZIP

# symlink .emacs.d
cd $HOME
ln -s $TARGET_DIR $HOME/.emacs.d

# greet
echo "dot-emacs is installed and ready to go. Start emacs now."
