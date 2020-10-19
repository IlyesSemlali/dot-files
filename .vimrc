" Use all the greatest and latest features of VIM
set nocompatible

" Vundle requirements
filetype off

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

call vundle#end()
filetype plugin indent on
" End of Vundle requirements


"""""""""""""""""""""""""""""
" Full custom configuration "
"""""""""""""""""""""""""""""

" Powerline fonts
set guifont=DroidSansMono\ Nerd\ Font\ 14
let g:airline_left_sep = "\uE0CC"
let g:airline_right_sep = "\uE0CC"
let g:airline_section_z = airline#section#create(["\uE0A1 " . '%{line(".")}' . " \uE0A3 " . '%{col(".")}'])

set splitright

" Automatic reload of .vimrc
autocmd! bufwritepost .vimrc source %

" Line numbers
set relativenumber
set nu
set undofile
nnoremap <leader>r :syn sync fromstart<CR>
" add yaml stuffs
au! BufNewFile,BufReadPost *.{yaml,yml} set filetype=yaml foldmethod=indent foldlevel=20
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

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

" handle indent text object
onoremap <silent>ai :<C-U>cal <SID>IndTxtObj(0)<CR>
onoremap <silent>ii :<C-U>cal <SID>IndTxtObj(1)<CR>
vnoremap <silent>ai :<C-U>cal <SID>IndTxtObj(0)<CR><Esc>gv
vnoremap <silent>ii :<C-U>cal <SID>IndTxtObj(1)<CR><Esc>gv

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



"" TODO

" Rice up vim so it rocks when coding python
" checkout he YT video called "Vim as a Python IDE - Martin Brochhaus"
