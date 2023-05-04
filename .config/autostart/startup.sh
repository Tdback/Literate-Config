#!/usr/bin/env bash

# Set keyboard speed
xset r rate 350 40

# Start Emacs daemon
if [[ ! $(ps aux | grep -q "[e]macs --daemon") ]]; then
  emacs --daemon 
fi
