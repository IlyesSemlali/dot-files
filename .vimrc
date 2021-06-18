let vim_logo = [
	\'                                       .                    ',
	\'                       ##############..... ##############   ',
	\'                       ##############......##############   ',
	\'                         ##########..........##########     ',
	\'                         ##########........##########       ',
	\'                         ##########.......##########        ',
	\'                         ##########.....##########..        ',
	\'                         ##########....##########.....      ',
	\'                       ..##########..##########.........    ',
	\'                     ....##########.#########.............  ',
	\'                       ..################JJJ............    ',
	\'                         ################.............      ',
	\'                         ##############.JJJ.JJJJJJJJJJ      ',
	\'                         ############...JJ...JJ..JJ  JJ     ',
	\'                         ##########....JJ...JJ..JJ  JJ      ',
	\'                         ########......JJJ..JJJ JJJ JJJ     ',
	\'                         ######    .........                ',
	\'                                     .....                  ',
	\'                                       .                    ']

""""""""""""""""""""""""""""
" Native vim configuration "
""""""""""""""""""""""""""""

" Use all the greatest and latest features of VIM
syntax on
set nocompatible
set modeline
set relativenumber
set nu
set nobackup
set noswapfile
set undofile
set undodir=~/.vim/undo
set hidden
set updatetime=2000
set splitright
set spelllang=fr

let &tags=expand('%:p:h') . "/.tags"
" Pasting options (to be tested)
set pastetoggle=<F2>
" set clipboard=unnamed

" " Column Color
" set colorcolumn=80
" highlight ColorColumn ctermbg=red

" set path and wildmenu to find all files under cwd
set path+=**
set wildmenu

" Automatic reload of .vimrc
autocmd! bufwritepost .vimrc source %


"""""""""""""""""""""""
" Vundle requirements "
"""""""""""""""""""""""
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

source ~/.vim/plugins.vim

call vundle#end()
filetype plugin indent on


"""""""""""""""""""""""""
" Plugins configuration "
"""""""""""""""""""""""""

" (built-in netrw)
let netrw_banner=0


" colorscheme
set background=dark
autocmd VimEnter * hi Normal ctermbg=none

hi! Normal ctermbg=NONE guibg=NONE
hi! NonText ctermbg=NONE guibg=NONE
colorscheme ghdark

let g:gh_color = "soft"


" Rooter
let g:rooter_patterns = ['=.terraform']

" Git Gutter
let g:gitgutter_enabled = 0


" Powerline fonts
let g:airline_left_sep = "\uE0CC"
let g:airline_right_sep = "\uE0CC"
let g:airline_section_z = airline#section#create(["\uE0A1 " . '%{line(".")}' . " \uE0A3 " . '%{col(".")}'])


" ALE
let g:airline#extensions#ale#enabled = 1
let g:ale_linters = {
			\   'python': ['flake8'],
			\}


" CtrlP
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = "rc"
let g:ctrlp_root_markers = ["terraform"]


" Startify
let g:startify_custom_header = vim_logo
let g:startify_bookmarks = ["~/.vimrc", "~/.xmonad/lib/"]
let g:startify_lists = [
	\ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
	\ { 'type': 'dir',       'header': ['   MRU '. getcwd()] },
	\ { 'type': 'sessions',  'header': ['   Sessions']       },
	\ ]


" CoC
source ~/.vim/coc.vim


""""""""""""""""
" Key Bindings "
""""""""""""""""

let mapleader = '!'
nnoremap <leader>r :syn sync fromstart<CR>
nnoremap <leader>! :nohl<CR>
nnoremap <leader>$ mz:%s/\s\+$//<CR>:nohl<CR>`zzz
nnoremap <leader>u :UndotreeShow<CR>
nnoremap <leader>m :MaximizerToggle<CR>
nnoremap <leader><SPACE> :Lex <bar> :vertical resize 30<CR>

" git related bindings
nnoremap <leader>gg :GitGutterToggle<CR>
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gs :Gstatus<CR>

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

" Move around errors
nnoremap <silent> <C-k> <Plug>(ale_previous_wrap)
nnoremap <silent> <C-j> <Plug>(ale_next_wrap)

" NetRW
noremap <silent> <C-N> :call ToggleNetrw()<CR>

" Tagbar
nnoremap <silent> <Leader>b :TagbarToggle<CR>

""""""""""""""""""""""""""
" Add some logic into it "
""""""""""""""""""""""""""

" NetRW
let g:NetrwIsOpen=0

function! ToggleNetrw()
    if g:NetrwIsOpen
        let i = bufnr("$")
        while (i >= 1)
            if (getbufvar(i, "&filetype") == "netrw")
                silent exe "bwipeout " . i
            endif
            let i-=1
        endwhile
        let g:NetrwIsOpen=0
    else
        let g:NetrwIsOpen=1
        silent 30Lexplore
    endif
endfunction

if (match ('xmonad', expand('%:p:h') > 0))
	let ale_haskell_ghc_options='-fno-code -v0 -i ~/.xmonad/lib/'
endif

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

augroup autocom
    autocmd!
    "execute the command on write
    autocmd VimLeave *.tf silent !terraform fmt %
augroup END

