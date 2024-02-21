local overrides = require("custom.configs.overrides")

---@type NvPluginSpec[]
local plugins = {

  -- Override plugin definition options

  {
    "neovim/nvim-lspconfig",
    dependencies = {
      -- format & linting
      {
        "jose-elias-alvarez/null-ls.nvim",
        config = function()
          require "custom.configs.null-ls"
        end,
      },
    },
    config = function()
      require "plugins.configs.lspconfig"
      require "custom.configs.lspconfig"
    end, -- Override to setup mason-lspconfig
  },

  -- override plugin configs
  {
    "williamboman/mason.nvim",
    opts = overrides.mason
  },

  {
    "nvim-treesitter/nvim-treesitter",
    opts = overrides.treesitter,
  },

  {
    "nvim-tree/nvim-tree.lua",
    lazy = false,
    opts = overrides.nvimtree,
  },

  {
    "editorconfig/editorconfig-vim",
    lazy = false
  },

  {
    "machakann/vim-sandwich",
    lazy = false
  },

  {
    "mbbill/undotree",
    lazy = false
  },

  {
    "towolf/vim-helm",
    lazy = false
  },

  {
    "gentoo/gentoo-syntax",
    lazy = false
  },

  {
    "hashivim/vim-terraform",
    ft = {"terraform", "hcl"}
  },

  {
    "neovimhaskell/haskell-vim",
    lazy = false
  },
  {
    "hrsh7th/nvim-cmp",
    lazy = false,
    opts = overrides.cmp
  },
  {
    "tpope/vim-fugitive",
    lazy = false
  },
  {
    "nanotee/zoxide.vim",
    lazy = false,
    dependencies = {
      "junegunn/fzf",
      lazy = false
    }
  },

  -- To make a plugin not be loaded
  -- {
  --   "NvChad/nvim-colorizer.lua",
  --   enabled = false
  -- },

  -- All NvChad plugins are lazy-loaded by default
  -- For a plugin to be loaded, you will need to set either `ft`, `cmd`, `keys`, `event`, or set `lazy = false`
  -- If you want a plugin to load on startup, add `lazy = false` to a plugin spec, for example
  -- {
  --   "mg979/vim-visual-multi",
  --   lazy = false,
  -- }
  {
    "xiyaowong/transparent.nvim",
    lazy = false,

    opts = {
      {
        enabled = false;
        groups = { -- table: default groups
          'Normal', 'NormalNC', 'Comment', 'Constant', 'Special', 'Identifier',
          'Statement', 'PreProc', 'Type', 'Underlined', 'Todo', 'String', 'Function',
          'Conditional', 'Repeat', 'Operator', 'Structure', 'LineNr', 'NonText',
          'SignColumn', 'CursorLineNr', 'EndOfBuffer',
        },
        extra_groups = {
          "NormalFloat", -- plugins which have float panel such as Lazy, Mason, LspInfo
          "NvimTreeNormal" -- NvimTree
        },
        exclude_groups = {}, -- table: groups you don't want to clear
      }
    }
  }
}

return plugins
