"""""""""""""""
" VIM Plugins "
"""""""""""""""

Plug 'Gavinok/SpaceWay.vim'
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'ap/vim-css-color'
Plug 'beloglazov/vim-online-thesaurus'
Plug 'dense-analysis/ale'
Plug 'editorconfig/editorconfig-vim'
Plug 'gentoo/gentoo-syntax'
Plug 'hashivim/vim-terraform'
Plug 'lilydjwg/colorizer'
Plug 'martinda/Jenkinsfile-vim-syntax'
Plug 'mbbill/undotree'
Plug 'mhinz/vim-startify'
Plug 'pearofducks/ansible-vim'
Plug 'preservim/tagbar'
Plug 'rkitover/vimpager'
Plug 'rust-lang/rust.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'preservim/nerdtree', { 'on':  'NERDTreeToggleVCS' }
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

if has('nvim')
    " Completion
    Plug 'onsails/lspkind-nvim'
    Plug 'neovim/nvim-lspconfig'
    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'hrsh7th/cmp-buffer'
    Plug 'hrsh7th/cmp-cmdline'
    Plug 'hrsh7th/nvim-cmp'
    Plug 'hrsh7th/cmp-vsnip'
    Plug 'hrsh7th/vim-vsnip'
    Plug 'tjdevries/colorbuddy.nvim'

    " Telescope
    Plug 'nvim-telescope/telescope-fzy-native.nvim'
    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-telescope/telescope.nvim'
endif
