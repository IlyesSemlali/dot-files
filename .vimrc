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

if exists("FindRootDirectory")
	autocmd VimEnter * let &tags=FindRootDirectory() . "/.tags"
endif

" Pasting options (to be tested)
set pastetoggle=<F2>

" set path and wildmenu to find all files under cwd
set path+=**
set wildmenu

" Automatic reload of .vimrc
autocmd! bufwritepost .vimrc source %
autocmd! bufwritepost plugins.vim source ~/.vimrc


"""""""""""""""""""""""
"       VIMPLUG       "
"""""""""""""""""""""""

call plug#begin('~/.vim/plugged')

source ~/.vim/plugins.vim

call plug#end()


"""""""""""""""""""""""""
" Plugins configuration "
"""""""""""""""""""""""""

" (built-in netrw)
let netrw_banner=0


" colorscheme
colorscheme spaceway
let g:airline_theme='ravenpower'

" Rooter
let g:rooter_patterns = ['.terraform', '.cloud', 'Chart.yaml']

" Git Gutter
let g:gitgutter_enabled = 0


" Powerline
let g:airline_section_z = airline#section#create(["\uE0A1 " . '%{line(".")}' . " \uE0A3 " . '%{col(".")}'])


" ALE
let g:airline#extensions#ale#enabled = 1
let g:ale_linters = {
			\   'python': ['flake8'],
			\}


" CtrlP
let g:ctrlp_cmd = 'CtrlPBuffer'
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


" Terraform
let g:terraform_fmt_on_save=1

" TagBar
let g:tagbar_type_terraform = {
    \ 'ctagstype' : 'terraform',
    \ 'kinds' : [
        \ 'r:Resource',
        \ 'd:Data',
        \ 'v:Variable',
        \ 'p:Provider',
        \ 'm:Module',
        \ 'o:Output',
        \ 'f:TFVar'
    \ ],
    \ 'sort' : 0
\ }

" NerdTree
let NERDTreeQuitOnOpen=1
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists('s:std_in') |
    \ execute 'NERDTree' argv()[0] | wincmd p | enew | execute 'cd '.argv()[0] | endif

" editorconfig
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

""""""""""""""""
" Key Bindings "
""""""""""""""""

let mapleader = ' '
nnoremap <leader>r :syn sync fromstart<CR>
nnoremap <silent><leader>n :nohl<CR>
nnoremap <silent><leader>$ mz:%s/\s\+$//<CR>:nohl<CR>`zz
nnoremap <leader>u :UndotreeToggle<CR>
nnoremap <leader>m :MaximizerToggle<CR>

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

" NerdTree
nnoremap <silent> <leader><space> :NERDTreeToggle<CR>

" Tagbar
nnoremap <silent> <leader>b :TagbarToggle<CR>

" Add borders on # based comments
nnoremap <silent> <leader>d A<space>#<esc>yyP:s/./#/g<CR>jp:s/./#/g<CR>:nohl<CR>

""""""""""""""""""""""""""
" Add some logic into it "
""""""""""""""""""""""""""

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
