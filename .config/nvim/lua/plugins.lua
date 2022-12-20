return(function(use)
    -- Completion
    -- use 'onsails/lspkind-nvim'
    -- use 'hrsh7th/cmp-buffer'
    -- use 'hrsh7th/cmp-cmdline'
    -- use 'hrsh7th/cmp-vsnip'
    -- use 'hrsh7th/vim-vsnip'

    -- Package manager
    use 'wbthomason/packer.nvim'

    use { -- LSP Configuration & Plugins
        'neovim/nvim-lspconfig',
        requires = {
            -- Automatically install LSPs to stdpath for neovim
            'williamboman/mason.nvim',
            'williamboman/mason-lspconfig.nvim',
        },
    }

    use { -- Autocompletion
        'hrsh7th/nvim-cmp',
        requires = { 'hrsh7th/cmp-nvim-lsp', 'L3MON4D3/LuaSnip', 'saadparwaiz1/cmp_luasnip' },
    }

    use { -- Highlight, edit, and navigate code
        'nvim-treesitter/nvim-treesitter',
        run = function()
            pcall(require('nvim-treesitter.install').update { with_sync = true })
        end,
    }

    use { -- Additional text objects via treesitter
        'nvim-treesitter/nvim-treesitter-textobjects',
        after = 'nvim-treesitter',
    }

    -- Git related plugins
    use 'tpope/vim-fugitive'
    use 'tpope/vim-rhubarb'
    use 'lewis6991/gitsigns.nvim'

    use 'gavinok/spaceway.vim'
    use 'nvim-lualine/lualine.nvim' -- Fancier statusline
    use 'lukas-reineke/indent-blankline.nvim' -- Add indentation guides even on blank lines
    use 'numToStr/Comment.nvim' -- "gc" to comment visual regions/lines
    use 'tpope/vim-sleuth' -- Detect tabstop and shiftwidth automatically

    -- Fuzzy Finder (files, lsp, etc)
    use { 'nvim-telescope/telescope.nvim', branch = '0.1.x', requires = { 'nvim-lua/plenary.nvim' } }

    -- Fuzzy Finder Algorithm which requires local dependencies to be built. Only load if `make` is available
    use { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make', cond = vim.fn.executable 'make' == 1 }


    -- use 'Gavinok/SpaceWay.vim'
    -- use 'airblade/vim-rooter'
    use 'ap/vim-css-color'
    -- use 'beloglazov/vim-online-thesaurus'
    use 'christianrondeau/vim-base64'
    use 'editorconfig/editorconfig-vim'
    use 'gentoo/gentoo-syntax'
    use 'hashivim/vim-terraform'
    -- use 'lilydjwg/colorizer'
    use 'machakann/vim-sandwich'
    use 'martinda/Jenkinsfile-vim-syntax'
    use 'mbbill/undotree'
    use 'mhinz/vim-startify'
    -- use 'rkitover/vimpager'
    use 'rust-lang/rust.vim'
    use 'ryanoasis/vim-devicons'
    use 'samoshkin/vim-mergetool'
    use 'towolf/vim-helm'
    use 'tpope/vim-eunuch'
    -- use 'vim-airline/vim-airline'
    -- use 'vim-airline/vim-airline-themes'


    use {
        'nvim-tree/nvim-tree.lua',
        requires = {
            'nvim-tree/nvim-web-devicons', -- optional, for file icons
        }
    }

end)
