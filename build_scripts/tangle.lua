#!/usr/bin/env lua

--[[
    Author  : Tyler Dunneback
    Date    : Jul 02, 2023
    GitHub  : https://github.com/Tdback
    Version : 1.0
--]]

-- Main script for tangling out dotfiles.
local lfs = require("lfs")

local directories = {
   ".config/",
   ".config/alacritty/",
   ".config/autostart/",
   ".config/bspwm/",
   ".config/sxhkd/",
   ".config/polybar/",
   ".config/rofi/"
}

for _, path in ipairs(directories) do
   local p = os.getenv("HOME") .. "/" .. path
   if not lfs.attributes(p, "mode") then
      print("Directory '" .. p .. "' not found, creating it now...")
      lfs.mkdir(p)
   end
end

-- Run tangle script
print("Creating temporary directory...")
lfs.mkdir(".temp/")

os.execute("emacs -Q --script tangle_helper.el")

print("Cleaning up temporary directory and tangle dependencies...")
os.execute("rm -rf .temp/")
