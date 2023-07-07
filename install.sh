#!/usr/bin/env bash

# Check if Script is Run as Root
if [[ $EUID -ne 0 ]]; then
  echo "You must be a root user to run this script, please run sudo ./install.sh" 2>&1
  exit 1
fi

username=$(id -u -n 1000)

# Install dependencies and programs used in configuration based on distro
if [[ $(uname -a) = *arch* ]]; then
  pacman -S lua luarocks emacs wget unzip bspwm sxhkd rofi polybar alacritty
else
  apt install lua5.4 luarocks emacs wget unzip bspwm sxhkd rofi polybar alacritty 
fi

# Install lua dependency for tangle script
luarocks install luafilesystem

# Install FantasqueSansM Nerd Font
mkdir -p /home/$username/.fonts/
wget "https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.2/FantasqueSansMono.zip"
wget "https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.2/JetBrainsMono.zip"
unzip font.zip -d /home/$username/.fonts/
unzip font_two.zip -d /home/$username/.fonts/
rm font*.zip
chown $username:$username /home/$username/.fonts/*

# Run the tangle script once after installation
cd /home/$username/.dotfiles/build_scripts/
chmod +x tangle.lua
su $username -c "./tangle.lua"

printf '%.s-' {1..80}
echo -e "\nNOTE: Some packages must be installed manually:"
echo -e "\tbetterlockscreen"
echo -e "\tstarship"
