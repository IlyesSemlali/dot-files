---@diagnostic disable: undefined-global
require "nvchad.mappings"

--@type MappingsTable
local map = vim.keymap.set

-- disable saving with control-s
map("n", "<C-s>", "")

-- disable line wrap workarounds
-- map("n", "j", "")
-- map("n", "k", "")
-- map("n", "<Up>", "")
-- map("n", "<Down>", "")

-- disable line numbers mappings
map("n", "<leader>n", "")
map("n", "<leader>rn", "")

-- disable going to beginning and end
map("i", "<C-b>", "")
map("i", "<C-e>", "")

-- disable line wrap workarounds
-- map("v", "<Up>", "")
-- map("v", "<Down>", "")

-- General mappings
map("n", "<Esc>", ":noh<CR>", { desc = "Clear highlights" })
map("n", "<leader>ch", "<cmd> NvCheatsheet <CR>", { desc = "Mapping cheatsheet" })

-- Telescope
local telescope = require "telescope.builtin"
map("n", "<leader>ff", telescope.find_files, { desc = "Find all" })
map("n", "<leader>fg", telescope.live_grep, { desc = "Live grep" })
map("n", "<space>e", telescope.diagnostics)
map("n", "[d", vim.diagnostic.goto_prev)
map("n", "]d", vim.diagnostic.goto_next)
map("n", "<leader>q", vim.diagnostic.setloclist)

-- LuaSnip
map({ "i", "s" }, "<C-S>", function()
  require("luasnip").jump(1)
end, { silent = true })

map({ "i", "s" }, "<C-X>", function()
  require("luasnip").jump(-1)
end, { silent = true })

-- Undo-Tree
map("n", "<leader>u", "<cmd> UndotreeToggle <CR>", { desc = "Open UndoTree" })

-- NVim-Tree
map("n", "<leader><space>", "<cmd> NvimTreeToggle <CR>", { desc = "Toggle NvimTree" })

-- Window navigation
map("n", "<C-h>", "<C-w>h", { desc = "Window left" })
map("n", "<C-l>", "<C-w>l", { desc = "Window right" })
map("n", "<C-j>", "<C-w>j", { desc = "Window down" })
map("n", "<C-k>", "<C-w>k", { desc = "Window up" })

-- Copy all
map("n", "<C-c>", "<cmd> %y+ <CR>", { desc = "Copy whole file" })

-- Indentation
map("v", "<", "<gv", { desc = "Indent line" })
map("v", ">", ">gv", { desc = "Indent line" })

return M
