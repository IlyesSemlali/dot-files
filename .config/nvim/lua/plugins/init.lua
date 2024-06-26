local overrides = require "configs.overrides"

local plugins = {

  {
    "neovim/nvim-lspconfig",
    dependencies = {
      -- format & linting
      {
        "jose-elias-alvarez/null-ls.nvim",
      },
    },
    opts = require "configs.lspconfig",
  },

  -- override plugin configs
  {
    "williamboman/mason.nvim",
    opts = overrides.mason,
  },

  {
    "nvim-telescope/telescope.nvim",
    branch = "0.1.x",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = require "configs.telescope",
  },

  {
    "stevearc/conform.nvim",
    lazy = false,
    opts = overrides.conform,
  },

  {
    "hrsh7th/nvim-cmp",
    lazy = false,
    opts = require "configs.cmp",
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
    lazy = false,
  },

  {
    "machakann/vim-sandwich",
    lazy = false,
  },

  {
    "mbbill/undotree",
    lazy = false,
  },

  {
    "towolf/vim-helm",
    ft = "helm",
  },

  {
    "gentoo/gentoo-syntax",
    lazy = false,
  },

  {
    "hashivim/vim-terraform",
    ft = { "terraform", "hcl" },
  },

  {
    "neovimhaskell/haskell-vim",
    lazy = false,
  },

  {
    "tpope/vim-fugitive",
    lazy = false,
  },

  {
    "nanotee/zoxide.vim",
    lazy = false,
    dependencies = {
      "junegunn/fzf",
      lazy = false,
    },
  },

  {
    "xiyaowong/transparent.nvim",
    lazy = false,
    opts = overrides.transparent,
  },
}

return plugins
