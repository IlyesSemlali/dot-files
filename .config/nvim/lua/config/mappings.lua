local map = vim.keymap.set

-- Buffer related mappings
map("n", "<Esc>", ":noh<CR>", { desc = "Clear highlights" })
map("n", "<leader>x", ":bd<CR>", { desc = "Close Buffer" })
map("n", "<C-c>", "<cmd> %y+ <CR>", { desc = "Copy whole file" })

-- Window navigation
map("n", "<C-h>", "<C-w>h", { desc = "Window left" })
map("n", "<C-l>", "<C-w>l", { desc = "Window right" })
map("n", "<C-j>", "<C-w>j", { desc = "Window down" })
map("n", "<C-k>", "<C-w>k", { desc = "Window up" })

-- Indentation
map("v", "<", "<gv", { desc = "Indent line" })
map("v", ">", ">gv", { desc = "Indent line" })

-- LSP
map("n", "<leader>ca", function()
	vim.lsp.buf.code_action()
end, { desc = "LSP Code Actions" })
