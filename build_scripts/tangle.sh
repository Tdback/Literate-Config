#!/bin/sh

# ===============================================================================
# 
#   Author  : Tyler Dunneback
#   Date    : Jun 15, 2023
#   GitHub  : https://github.com/Tdback
#   Version : 0.2
# 
# ===============================================================================

# Main script for tangling out dotfiles.

# Add .config directory for each program
if [[ ! -d ~/.config/ ]]; then
  echo "Directory '~/.config/' not found, creating it now..."
  mkdir -p ~/.config/
fi

# Check if alacritty directory exists. If not, create it.
if [[ ! -d ~/.config/alacritty/ ]]; then
  echo "Directory '~/.config/alacritty/' not found, creating it now..."
  mkdir -p ~/.config/alacritty/ 
fi

# Check if autostart directory exists. If not, create it.
if [[ ! -d ~/.config/autostart/ ]]; then
  echo "Directory '~/.config/autostart/' not found, creating it now..."
  mkdir -p ~/.config/autostart/ 
fi

# Check if bspwm directory exists. If not, create it.
if [[ ! -d ~/.config/bspwm/ ]]; then
  echo "Directory '~/.config/bspwm/' not found, creating it now..."
  mkdir -p ~/.config/bspwm/ 
fi

# Check if sxhkd directory exists. If not, create it.
if [[ ! -d ~/.config/sxhkd/ ]]; then
  echo "Directory '~/.config/sxhkd/' not found, creating it now..."
  mkdir -p ~/.config/sxhkd/ 
fi

# Check if polybar directory exists. If not, create it.
if [[ ! -d ~/.config/polybar/ ]]; then
  echo "Directory '~/.config/polybar/' not found, creating it now..."
  mkdir -p ~/.config/polybar/ 
fi

# Check if rofi directory exists. If not, create it.
if [[ ! -d ~/.config/rofi/ ]]; then
  echo "Directory '~/.config/rofi/' not found, creating it now..."
  mkdir -p ~/.config/rofi/ 
fi

# Run tangle script.
echo "Creating temporary directory..."
mkdir -p .temp/

emacs -Q --script tangle_helper.el

echo "Cleaning up dependencies and temporary directory..."
rm -rf .temp/
