-- Here are stored plugin configurations that need only a few lines
-- to be set up properly. If it takes more than 10 lines, use a
-- seperate file

-- Enable Comment.nvim
local comment_loaded,comment = pcall(require, 'Comment')
if comment_loaded
then
  comment.setup()
end

-- Enable `lukas-reineke/indent-blankline.nvim`
-- See `:help indent_blankline.txt`
local indent_loaded, indent = pcall(require, 'indent_blankline')
if indent_loaded
then
  indent.setup {
    char = '┊',
    show_trailing_blankline_indent = false,
  }
end

-- Gitsigns
-- See `:help gitsigns.txt`
local gitsigns_loaded, gitsigns =  pcall(require, 'gitsigns')
if gitsigns_loaded
then
  gitsigns.setup {
    signs = {
      add = { text = '+' },
      change = { text = '~' },
      delete = { text = '_' },
      topdelete = { text = '‾' },

      changedelete = { text = '~' },
    },
  }
end

-- Set lualine as statusline
-- See `:help lualine.txt`
local lualine_loaded,lualine = pcall(require, 'lualine')
if lualine_loaded
then
  lualine.setup {
    options = {
      icons_enabled = true,
      theme = 'jellybeans',
      component_separators = { left = '', right = ''},
      section_separators = { left = '', right = ''},
    },
  }
end
