local autocmd = vim.api.nvim_create_autocmd

-- Auto resize panes when resizing nvim window
-- autocmd("VimResized", {
--   pattern = "*",
--   command = "tabdo wincmd =",
-- })

-- Remove trailing spaces when saving file
autocmd({ "BufWritePre" }, {
	pattern = { "*" },
	command = [[silent! %s/\s\+$//]],
})
