local autocmd = vim.api.nvim_create_autocmd

-- Auto resize panes when resizing nvim window
-- autocmd("VimResized", {
--   pattern = "*",
--   command = "tabdo wincmd =",
-- })

-- Restore cursor position
autocmd({ "BufReadPost" }, {
  pattern = { "*" },
  callback = function()
    vim.api.nvim_exec('silent! normal! g`"zv', false)
  end,
})

-- Remove trailing spaces when saving file
autocmd({ "BufWritePre" }, {
  pattern = { "*" },
  command = [[silent! %s/\s\+$//]],
})

-- Automatically close NvimTree on quit
-- from https://github.com/nvim-tree/nvim-tree.lua/wiki/Auto-Close
autocmd({ "QuitPre" }, {
  callback = function()
    vim.cmd "NvimTreeClose"
  end,
})
