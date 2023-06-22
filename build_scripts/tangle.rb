#!/usr/bin/env ruby

# ===============================================================================
# 
#   Author  : Tyler Dunneback
#   Date    : Jun 15, 2023
#   GitHub  : https://github.com/Tdback
#   Version : 1.0
# 
# ===============================================================================

# Main script for tangling out dotfiles.
require 'fileutils'

directories = [
  File.expand_path "~/.config/",
  File.expand_path "~/.config/alacritty/",
  File.expand_path "~/.config/autostart/",
  File.expand_path "~/.config/bspwm/",
  File.expand_path "~/.config/sxhkd/",
  File.expand_path "~/.config/polybar/",
  File.expand_path "~/.config/rofi/"
]

directories.each { |path|
  unless Dir.exist?(path)
    puts "Directory '#{path}' not found, creating it now..."
    Dir.mkdir path
  end
}

# Run tangle script.
puts "Creating temporary directory..."
Dir.mkdir ".temp/"

system "emacs -Q --script tangle_helper.el"

puts "Cleaning up dependencies and temporary directory..."
FileUtils.rm_r ".temp/", :force => true, :secure => true
