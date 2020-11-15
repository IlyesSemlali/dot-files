""""""""""""""""""""""""""""
" Native vim configuration "
""""""""""""""""""""""""""""

" Use all the greatest and latest features of VIM
set nocompatible
set relativenumber
set nu
set undofile

set updatetime=2000
set splitright

" Pasting options (to be tested)
set pastetoggle=<F2>
set clipboard=unnamed

" Column Color
set colorcolumn=80
highlight ColorColumn ctermbg=0 guibg=lightgrey

" set path and wildmenu to find all files under cwd
set path+=**
set wildmenu

" Vundle requirements
filetype off

" Automatic reload of .vimrc
autocmd! bufwritepost .vimrc source %

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Lists of all plugins that should be installed
Plugin 'VundleVim/Vundle.vim'

Plugin 'beloglazov/vim-online-thesaurus'
Plugin 'martinda/Jenkinsfile-vim-syntax'
Plugin 'pearofducks/ansible-vim'
Plugin 'vim-airline/vim-airline'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-surround'
Plugin 'lilydjwg/colorizer'
Plugin 'rust-lang/rust.vim'
Plugin 'rkitover/vimpager'
Plugin 'dense-analysis/ale'
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-eunuch'
Plugin 'vim-utils/vim-man'
Plugin 'mbbill/undotree'

call vundle#end()
filetype plugin indent on
" End of Vundle requirements

"""""""""""""""""""""""""
" Plugins configuration "
"""""""""""""""""""""""""
" Git Gutter
let g:gitgutter_enabled = 0

" Powerline fonts
set guifont=DroidSansMono\ Nerd\ Font\ 14
let g:airline_left_sep = "\uE0CC"
let g:airline_right_sep = "\uE0CC"
let g:airline_section_z = airline#section#create(["\uE0A1 " . '%{line(".")}' . " \uE0A3 " . '%{col(".")}'])

" Leader based keybindings
let mapleader = ' '
nnoremap <leader>r :syn sync fromstart<CR>
nnoremap <leader>! :nohl<CR>
nnoremap <leader>d :call GitDiff()<CR>
nnoremap <leader>s :call GitStatus()<CR>
nnoremap <leader>g :GitGutterToggle<CR>
nnoremap <leader>$ mz:%s/\s\+$//<CR>:nohl<CR>`zzz
nnoremap <leader>u :UndotreeShow<CR>

" handle indent text object
onoremap <silent>ai :<C-U>cal <SID>IndTxtObj(0)<CR>
onoremap <silent>ii :<C-U>cal <SID>IndTxtObj(1)<CR>
vnoremap <silent>ai :<C-U>cal <SID>IndTxtObj(0)<CR><Esc>gv
vnoremap <silent>ii :<C-U>cal <SID>IndTxtObj(1)<CR><Esc>gv

" switch between windows
nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>

" Move selection
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" Custom functions
function! GitStatus()
	vert term git status
endfunction

function! GitDiff()
	vert term git --no-pager diff -- %
endfunction

function! s:IndTxtObj(inner)
	let curline = line(".")
	let lastline = line("$")
	let i = indent(line(".")) - &shiftwidth * (v:count1 - 1)
	let i = i < 0 ? 0 : i
	if getline(".") !~ "^\\s*$"
		let p = line(".") - 1
		let nextblank = getline(p) =~ "^\\s*$"
		while p > 0 && ((i == 0 && !nextblank) || (i > 0 && ((indent(p) >= i && !(nextblank && a:inner)) || (nextblank && !a:inner))))
			-
			let p = line(".") - 1
			let nextblank = getline(p) =~ "^\\s*$"
		endwhile
		normal! 0V
		call cursor(curline, 0)
		let p = line(".") + 1
		let nextblank = getline(p) =~ "^\\s*$"
		while p <= lastline && ((i == 0 && !nextblank) || (i > 0 && ((indent(p) >= i && !(nextblank && a:inner)) || (nextblank && !a:inner))))
			+
			let p = line(".") + 1
			let nextblank = getline(p) =~ "^\\s*$"
		endwhile
		normal! $
	endif
endfunction

" ALE settings
let g:airline#extensions#ale#enabled = 1
let g:ale_linters = {
			\   'python': ['flake8'],
			\}

"" TODO

" Install and configure keybindigs for szw/vim-maximizer

" Rice up vim so it rocks when coding python
" checkout he YT video called "Vim as a Python IDE - Martin Brochhaus"
