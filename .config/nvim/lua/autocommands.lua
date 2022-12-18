-- Restore cursor position
vim.api.nvim_create_autocmd({ "BufReadPost" }, {
    pattern = { "*" },
    callback = function()
        vim.api.nvim_exec('silent! normal! g`"zv', false)
    end,
})

-- Remove trailing spaces when saving file
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
  pattern = { "*" },
  command = [[silent! %s/\s\+$//e]],
})

-- Automatically source and re-compile packer whenever you save this init.lua
local packer_group = vim.api.nvim_create_augroup('Packer', { clear = true })
vim.api.nvim_create_autocmd('BufWritePost', {
  command = 'source <afile> | PackerCompile',
  group = packer_group,
  pattern = vim.fn.expand '$MYVIMRC',
})

-- TODO: Use array to list all items that need background removals
-- and set both autocommands and config
vim.api.nvim_create_autocmd(
    { "VimEnter" },
    { pattern = { "*" }, command = "highlight Normal guibg=NONE ctermbg=NONE" }
)

vim.api.nvim_create_autocmd(
    { "VimEnter" },
    { pattern = { "*" }, command = "highlight EndOfBuffer guibg=NONE ctermbg=NONE" }
)

vim.api.nvim_create_autocmd(
    { "VimEnter" },
    { pattern = { "*" }, command = "highlight NvimTreeNormal guibg=NONE ctermbg=NONE" }
)

vim.api.nvim_create_autocmd(
    { "VimEnter" },
    { pattern = { "*" }, command = "highlight NvimTreeEndOfBuffer guibg=NONE ctermbg=NONE" }
)

vim.api.nvim_create_autocmd(
    { "VimEnter" },
    { pattern = { "*" }, command = "highlight SignColumn guibg=NONE ctermbg=NONE" }
)

vim.api.nvim_create_autocmd(
    { "VimEnter" },
    { pattern = { "*" }, command = "highlight LineNr guibg=NONE ctermbg=NONE guifg=grey" }
)

vim.api.nvim_create_autocmd('BufWritePost', {
    command = 'highlight Normal guibg=NONE ctermbg=NONE',
    group = packer_group,
    pattern = vim.fn.expand '$MYVIMRC',
})

vim.api.nvim_create_autocmd('BufWritePost', {
    command = 'highlight SignColumn guibg=NONE ctermbg=NONE',
    group = packer_group,
    pattern = vim.fn.expand '$MYVIMRC',
})

vim.api.nvim_create_autocmd('BufWritePost', {
    command = 'highlight LineNr guibg=NONE',
    group = packer_group,
    pattern = vim.fn.expand '$MYVIMRC',
})
