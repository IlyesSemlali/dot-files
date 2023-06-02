return(function(use)
    -- Package manager
    use 'wbthomason/packer.nvim'

    use { -- Make neovim fancier
        'editorconfig/editorconfig-vim',
        'gavinok/spaceway.vim',
        'lukas-reineke/indent-blankline.nvim',
        'machakann/vim-sandwich',
        'mbbill/undotree',
        'mhinz/vim-startify',
        'numToStr/Comment.nvim',
        'nvim-lualine/lualine.nvim',
        'ryanoasis/vim-devicons',
        'tpope/vim-sleuth' -- Detect tabstop and shiftwidth automatically
    }

    use { -- Telescope
        'nvim-telescope/telescope-file-browser.nvim',
        requires = {
            'nvim-telescope/telescope.nvim',
            requires = { 'nvim-lua/plenary.nvim' }
        },
        { 'nvim-telescope/telescope-fzf-native.nvim',
            run = 'make',
            cond = vim.fn.executable 'make' == 1 }
    }

    use { -- shell commands (:Remove :Chmod, :Mkdir :SudoWrite ...)
        'tpope/vim-eunuch'
    }

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

    use { -- Additional text objects via treesitter
        'nvim-treesitter/nvim-treesitter-textobjects',
        after = 'nvim-treesitter',
    }

    use { -- Git
        'tpope/vim-fugitive',
        'tpope/vim-rhubarb',
        'lewis6991/gitsigns.nvim'
    }

    -- Languages and Syntaxes

    use 'ap/vim-css-color'
    use 'gentoo/gentoo-syntax'
    use 'hashivim/vim-terraform'
    use 'martinda/Jenkinsfile-vim-syntax'
    use 'rust-lang/rust.vim'
    use 'towolf/vim-helm'

    -- Try one of those sometime:

        -- use 'airblade/vim-rooter'
        -- use 'rhysd/vim-grammarous'
        -- use 'beloglazov/vim-online-thesaurus'
        -- use 'christianrondeau/vim-base64' -- Do I still use this ?
        -- use 'samoshkin/vim-mergetool' -- Broken

end)
