#!/usr/bin/env lua

--[[
   Author  : Tyler Dunneback
   Date    : Jul 12, 2023
   GitHub  : https://github.com/Tdback
   Version : 1.1
--]]

local function help()
   local help_msg = [[
Usage: ./tangle.lua [OPTION]...
Tangle out literate configuration files to respective config files.

With no OPTION, script creates temporary directory to use as future
caching mechanism.

  --clean    remove temporary directory `.temp/' and tangle dependencies.
  --help     display this help and exit.
  --version  output version information and exit.

Source repository: <https://github.com/Tdback/Literate-Config/>]]

   print(help_msg)
   os.exit()
end

local function version()
   local version_msg = [[
tangle.lua 1.1
License MIT: <https://mit-license.org/>

Written by Tyler "Tdback" Dunneback]]

   print(version_msg)
   os.exit()
end


-- Help page
if arg[1] == "--help" and arg[1] ~= "--clean" and arg[1] ~= "--version" then
   help()
end

-- Version page
if arg[1] == "--version" then
   version()
end

-- If user wants, they can clean up the temp directory
if arg[1] == "--clean" then
   print("Cleaning up temporary directory and tangle dependencies...")
   os.execute("rm -rf .temp/")
   os.exit()
end

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

-- Create any missing .config directories
for _, path in ipairs(directories) do
   local p = os.getenv("HOME") .. "/" .. path
   if not lfs.attributes(p, "mode") then
      print("Directory '" .. p .. "' not found, creating it now...")
      lfs.mkdir(p)
   end
end

-- Create temporary directory as caching mechanism for tangling dependencies
if not lfs.attributes("./.temp/", "mode") then
   print("Creating temporary directory...")
   lfs.mkdir(".temp/")
end

-- Run tangle script
os.execute("emacs -Q --script tangle_helper.el")
