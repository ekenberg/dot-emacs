dot-emacs
=========

### My Emacs customizations

* I use a Swedish Keyboard
* I use Emacs on Mac OS X and Linux (Mint KDE)
* I use Emacs both in the terminal (no x) and with a gui (Cocoa / GTK)
* I use this config for all OS:es and Emacs-variants. One config to rule them all.

### Installation:

#### Automatic:
The commandline program ```unzip``` is required for installation. It's present by default on most Macs and Linuxes

    curl https://raw.github.com/ekenberg/dot-emacs/master/init.el | sh

or

    wget -qO - https://raw.github.com/ekenberg/dot-emacs/master/init.el | sh

#### Manual:

Clone this repo from github and symlink the directory:

    cd
    # backup old .emacs.d as/if needed:
    mv .emacs.d .emacs.d.old
    ln -s path-to-dot-emacs .emacs.d
