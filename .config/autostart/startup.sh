#!/usr/bin/env bash

# Set keyboard speed
xset r rate 350 40

# Set audio to headphones using pactl
# headphones="alsa_output.pci-0000_2d_00.4.analog-stereo"

# if echo $(pactl list short sinks) | grep -q "${headphones}" ; then
# 	pactl set-default-sink "${headphones}"
# fi
