#!/bin/sh

#  TODO:
#+ Add functionality to create files if directory doesn't exist for
#+ each dotfile.

echo "Creating temporary directory..."
mkdir -p ./.temp

emacs -Q --script tangler.el 

echo "Removing temporary directory..."
rm -rf ./.temp
