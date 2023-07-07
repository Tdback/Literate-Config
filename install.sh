#!/bin/sh

# Check if Script is Run as Root
if [[ $EUID -ne 0 ]]; then
  echo "You must be a root user to run this script, please run sudo ./install.sh" 2>&1
  exit 1
fi

# Install dependencies and programs used in configuration based on distro
if [[ $(uname -a) = *arch* ]]; then
  pacman -S lua luarocks emacs wget unzip bspwm sxhkd rofi polybar alacritty
else
  apt install lua5.4 luarocks emacs wget unzip bspwm sxhkd rofi polybar alacritty 
fi

#  Some packages need to be installed manually or through the Arch AUR:
#+ betterlockscreen
#+ starship.rs

# Install lua dependency for tangle script
luarocks install luafilesystem

# Install FantasqueSansM Nerd Font
mkdir -p ~/.fonts/
wget -O ~/.fonts/font.zip "https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.2/FantasqueSansMono.zip"
unzip ~/.fonts/font.zip
rm ~/.fonts/font.zip

# Create dotfiles directory and set up config files for tangling
mkdir -p ~/.dotfiles/ 
mv Literate-Config/* ~/.dotfiles/ 
rm -rf Literate-Config/

# Run the tangle script once after installation
cd ~/.dotfiles/build_scripts/
chmod +x tangle.lua
./tangle.lua
