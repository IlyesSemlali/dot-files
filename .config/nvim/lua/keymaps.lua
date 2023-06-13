-- [[ Basic Keymaps ]]

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })


-- Diagnostic keymaps
vim.keymap.set('n', '<leader>DE', vim.diagnostic.goto_prev)
vim.keymap.set('n', '<leader>de', vim.diagnostic.goto_next)
vim.keymap.set('n', '<leader>df', vim.diagnostic.open_float)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)

-- List maniuplation keymaps
vim.keymap.set('n', '<leader>lt', "kmoj:.m0<CR>'o")
vim.keymap.set('n', '<leader>lb', "jmok:.m$<CR>'o")

-- Nvim-Tree bookmarks
vim.keymap.set("n", "<leader>mn", require("nvim-tree.api").marks.navigate.next)
vim.keymap.set("n", "<leader>mp", require("nvim-tree.api").marks.navigate.prev)
vim.keymap.set("n", "<leader>ms", require("nvim-tree.api").marks.navigate.select)
