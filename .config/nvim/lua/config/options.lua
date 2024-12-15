local option = vim.o
local global = vim.g

global.mapleader = " "
global.maplocalleader = "\\"

option.number = true
option.relativenumber = true
option.whichwrap = "bs"

option.clipboard = ""
option.undofile = true

option.tabstop = 2 -- A TAB character looks like 4 spaces
option.expandtab = true -- Pressing the TAB key will insert spaces instead of a TAB character
option.softtabstop = 2 -- Number of spaces inserted instead of a TAB character
option.shiftwidth = 2 -- Number of spaces inserted when indenting
