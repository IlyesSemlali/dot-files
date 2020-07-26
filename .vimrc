" Use all the greatest and latest features of VIM
set nocompatible

" Vundle requirements
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Lists of all plugins that should be installed
Plugin 'VundleVim/Vundle.vim'
Plugin 'vim-airline/vim-airline'
Plugin 'tpope/vim-surround'
Plugin 'ap/vim-css-color'


call vundle#end()
filetype plugin indent on
" End of Vundle requirements


"""""""""""""""""""""""""""""
" Full custom configuration "
"""""""""""""""""""""""""""""

set splitright

" Automatic reload of .vimrc
autocmd! bufwritepost .vimrc source %

" Line numbers
set relativenumber
set nu

" Pasting options (to be tested)
set pastetoggle=<F2>
set clipboard=unnamed

" Disable highlighting after a search
" TODO: Use <leader> 
" noremap <C-n> :nohl<CR>
" vnoremap <C-n> :nohl<CR>
" inoremap <C-n> :nohl<CR>

" set path and wildmenu to find all files under cwd
set path+=**
set wildmenu

" Custom function

function! GitStatus()
	vert term git status
endfunction

function! GitDiff()
	vert terminal git diff -- %
endfunction

"" TODO

" Rice up vim so it rocks when coding python 
" checkout he YT video called "Vim as a Python IDE - Martin Brochhaus"
