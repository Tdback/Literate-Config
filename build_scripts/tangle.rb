#!/usr/bin/env ruby

# ===============================================================================
# 
#   Author  : Tyler Dunneback
#   Date    : Jun 22, 2023
#   GitHub  : https://github.com/Tdback
#   Version : 1.0
# 
# ===============================================================================

# Main script for tangling out dotfiles.
require 'fileutils'

directories = %w[
  ~/.config/
  ~/.config/alacritty/
  ~/.config/autostart/
  ~/.config/bspwm/
  ~/.config/sxhkd/
  ~/.config/polybar/
  ~/.config/rofi/
].map(&File.method(:expand_path))

directories.each { |path|
  unless Dir.exist? path 
    puts "Directory '#{path}' not found, creating it now..."
    Dir.mkdir path
  end
}

# Run tangle script.
puts "Creating temporary directory..."
Dir.mkdir ".temp/"

system "emacs -Q --script tangle_helper.el"

puts "Cleaning up dependencies and temporary directory..."
FileUtils.rm_rf ".temp/", :secure => true
