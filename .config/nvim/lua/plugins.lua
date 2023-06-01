return(function(use)
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
        requires = { 'hrsh7th/cmp-nvim-lsp',
            'L3MON4D3/LuaSnip',
            'saadparwaiz1/cmp_luasnip'
        },
    }

    use { -- Highlight, edit, and navigate code
        'nvim-treesitter/nvim-treesitter',
        run = function()
            pcall(require('nvim-treesitter.install').update { with_sync = true })
        end,
    }

    use { -- File Explorer
        'nvim-tree/nvim-tree.lua',
        requires = {
            'nvim-tree/nvim-web-devicons',
        }
    }


    use { -- Additional text objects via treesitter
        'nvim-treesitter/nvim-treesitter-textobjects',
        after = 'nvim-treesitter',
    }

    use { -- Git related plugins
        'tpope/vim-fugitive',
        'tpope/vim-rhubarb',
        'lewis6991/gitsigns.nvim'
    }

    use { -- Make neovim fancier
        'gavinok/spaceway.vim',
        'lukas-reineke/indent-blankline.nvim',
        'mhinz/vim-startify',
        'nvim-lualine/lualine.nvim',
        'ryanoasis/vim-devicons'
    }

    use 'editorconfig/editorconfig-vim'
    use 'numToStr/Comment.nvim' -- "gc" to comment visual regions/lines
    use 'tpope/vim-sleuth' -- Detect tabstop and shiftwidth automatically
    use 'mbbill/undotree'

    -- Fuzzy Finder (files, lsp, etc)
    use {
        "nvim-telescope/telescope-file-browser.nvim",
        requires = {
        'nvim-telescope/telescope.nvim',
            requires = { 'nvim-lua/plenary.nvim' }
        }
    }

    -- Fuzzy Finder Algorithm which requires local dependencies to be built. Only load if `make` is available
    use { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make', cond = vim.fn.executable 'make' == 1 }


    -- Try one of those somtime
    -- use 'airblade/vim-rooter'
    -- use 'beloglazov/vim-online-thesaurus'
    -- use 'christianrondeau/vim-base64' -- Do I still use this ?
    -- use 'samoshkin/vim-mergetool' -- Broken
    use 'ap/vim-css-color'
    use 'gentoo/gentoo-syntax'
    use 'hashivim/vim-terraform'
    use 'machakann/vim-sandwich'
    use 'martinda/Jenkinsfile-vim-syntax'
    use 'rust-lang/rust.vim'
    use 'towolf/vim-helm'
    use 'tpope/vim-eunuch'
    -- use 'vim-airline/vim-airline'
    -- use 'vim-airline/vim-airline-themes'



end)
