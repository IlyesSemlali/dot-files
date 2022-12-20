vim.diagnostic.disable()
vim.defer_fn(function()
    vim.diagnostic.reset()
end, 1000)
