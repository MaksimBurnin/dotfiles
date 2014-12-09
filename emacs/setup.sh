#!/bin/bash

set -e
SCRIPTDIR=`dirname "$BASH_SOURCE"`
SCRIPTPATH=`readlink -f "$SCRIPTDIR"`

cd ~
ln -s "$SCRIPTPATH/.emacs" ".emacs"
ln -s "$SCRIPTPATH/.emacs.d" ".emacs.d"
