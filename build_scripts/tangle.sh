#!/bin/sh

#  TODO:
#+ Add functionality to create files if directory doesn't exist for
#+ each dotfile.

echo "Creating temporary directory..."
mkdir -p .temp/

emacs -Q --script tangle_helper.el

echo "Cleaning up dependencies and temporary directory..."
rm -rf .temp/
