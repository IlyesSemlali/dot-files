local M = {}

-- Prevent error during the initial Lazy bootstrap:
--   Error detected while processing ~/.config/nvim/init.lua:
--   Failed to load `custom.plugins`
--   ~/.config/nvim/lua/custom/plugins.lua:1: loop or previous error loading module 'custom.configs.overrides'

M.treesitter = {
  ensure_installed = {
    "vim",
    "lua",
    "html",
    "css",
    "javascript",
    "typescript",
    "tsx",
    "c",
    "markdown",
    "markdown_inline",
  },
  indent = {
    enable = true,
    -- disable = {
    --   "python"
    -- },
  },
}

M.mason = {
  ensure_installed = {
    -- lua
    "lua-language-server",
    "stylua",

    -- web dev
    "css-lsp",
    "html-lsp",
    "prettier",

    -- c/cpp
    "clangd",
    "clang-format",

    -- python
  },

  ui = {
    icons = {
      package_installed = "✓",
      package_pending = "➜",
      package_uninstalled = "✗",
    },
  },
}

-- git support in nvimtree
M.nvimtree = {
  git = {
    enable = true,
    ignore = false,
  },

  renderer = {
    highlight_git = true,
    icons = {
      show = {
        git = true,
      },
    },
  },
}

-- Autoformat
M.conform = {
  formatters_by_ft = {
    lua = { "stylua" },
    -- css = { "prettier" },
    -- html = { "prettier" },
  },

  format_on_save = {
    -- These options will be passed to conform.format()
    timeout_ms = 500,
    lsp_fallback = true,
  },
}

M.transparent = {
  {
    enabled = false,
    groups = { -- table: default groups
      "Normal",
      "NormalNC",
      "Comment",
      "Constant",
      "Special",
      "Identifier",
      "Statement",
      "PreProc",
      "Type",
      "Underlined",
      "Todo",
      "String",
      "Function",
      "Conditional",
      "Repeat",
      "Operator",
      "Structure",
      "LineNr",
      "NonText",
      "SignColumn",
      "CursorLineNr",
      "EndOfBuffer",
    },
    extra_groups = {
      "NormalFloat", -- plugins which have float panel such as Lazy, Mason, LspInfo
      "NvimTreeNormal", -- NvimTree
    },
    exclude_groups = {}, -- table: groups you don't want to clear
  },
}

return M
