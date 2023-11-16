---@type MappingsTable
local M = {}

M.disabled = {
  n = {
    -- disable saving with control-s
    ["<C-s>"] = "",

    -- disable line wrap workarounds
    ["j"] = "",
    ["k"] = "",
    ["<Up>"] = "",
    ["<Down>"] = "",

    -- disable line numbers mappings
    ["<leader>n"] = "",
    ["<leader>rn"] = "",
  },

  i = {
    -- disable going to beginning and end
    ["<C-b>"] = "",
    ["<C-e>"] = "",
  },

  v = {
    -- disable line wrap workarounds
    ["<Up>"] = "",
    ["<Down>"] = "",
  },
}

M.general = {
  n = {
    -- General mappings
    ["<Esc>"] = { ":noh <CR>", "Clear highlights" },

    -- Telescope
    ["<leader>ff"] = { "<cmd> Telescope find_files follow=true no_ignore=true hidden=true <CR>", "Find all" },
    ["<leader>fg"] = { "<cmd> Telescope live_grep <CR>", "Live grep" },
    ["<leader>e"] = { "<cmd> Telescope live_grep <CR>", "Live grep" },

    -- Undo-Tree
    ["<leader>u"] = { "<cmd> UndotreeToggle <CR>", "Open UndoTree" },

    -- NVim-Tree
    ["<leader><space>"] = { "<cmd> NvimTreeFocus <CR>", "Open NvimTree" },

    -- switch between windows
    ["<C-h>"] = { "<C-w>h", "Window left" },
    ["<C-l>"] = { "<C-w>l", "Window right" },
    ["<C-j>"] = { "<C-w>j", "Window down" },
    ["<C-k>"] = { "<C-w>k", "Window up" },

    -- Copy all
    ["<C-c>"] = { "<cmd> %y+ <CR>", "Copy whole file" },

    -- new buffer
    ["<leader>b"] = { "<cmd> enew <CR>", "New buffer" },
    ["<leader>ch"] = { "<cmd> NvCheatsheet <CR>", "Mapping cheatsheet" },

    ["<leader>fm"] = {
      function()
        vim.lsp.buf.format { async = true }
      end,
      "LSP formatting",
    },
  },

  s = {
    -- LuaSnip
    ["<C-j>"] = { "<cmd>lua require'luasnip'.jump(1)<CR>", "Go to next snipped field" },
    ["<C-J>"] = { "<cmd>lua require'luasnip'.jump(-1)<CR>", "Go to next snipped field" },
  },

  i = {
    -- LuaSnip
    ["<C-j>"] = { "<cmd>lua require'luasnip'.jump(1)<CR>", "Go to next snipped field" },
    ["<C-J>"] = { "<cmd>lua require'luasnip'.jump(-1)<CR>", "Go to next snipped field" },
  },

  v = {
    ["<"] = { "<gv", "Indent line" },
    [">"] = { ">gv", "Indent line" },
  },

  -- TODO: Include Telescope Keymaps:
  -- https://github.com/IlyesSemlali/dot-files/blob/main/.config/nvim/plugin/telescope.lua
}

return M
