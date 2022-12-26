-- This config is based on https://github.com/nvim-lua/kickstart.nvim
-- It is soooo great, can't believe how much it's enjoyable to use it !

-- Set <space> as the leader key
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

require('plugin_manager')
require('configuration')
require('keymaps')
require('behavior')
require('autocommands')

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
