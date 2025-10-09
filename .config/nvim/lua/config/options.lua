local option = vim.o
local global = vim.g

-- Native Options
global.mapleader = " "
global.maplocalleader = "\\"
global.markdown_fenced_languages = {
	"bash",
	"json",
	"python",
	"terraform",
	"yaml",
}

option.number = true
option.relativenumber = true
option.signcolumn = "auto:1-2"

option.whichwrap = "bs"

option.clipboard = ""
option.undofile = true

option.tabstop = 2 -- A TAB character looks like 4 spaces
option.expandtab = true -- Pressing the TAB key will insert spaces instead of a TAB character
option.softtabstop = 2 -- Number of spaces inserted instead of a TAB character
option.shiftwidth = 2 -- Number of spaces inserted when indenting

-- Plugin Options
global.lastplace_ignore = "gitcommit,gitrebase" -- farmergreg/vim-lastplace

-- Disable unused providers (for nvim development)
global.loaded_node_provider = 0
global.loaded_perl_provider = 0
global.loaded_python3_provider = 0
global.loaded_ruby_provider = 0
