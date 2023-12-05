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
set noshowmode
set modeline
if $DEMO_MODE == ""
    set relativenumber
    set nu
endif
set nobackup
set noswapfile
set undofile
set undodir=~/.vim/undo
set hidden
set updatetime=2000
set splitright
set spelllang=fr

set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" if exists("FindRootDirectory")
"     autocmd VimEnter * let &tags=FindRootDirectory() . "/.tags"
" endif

" Startup
autocmd BufEnter * lcd %:p:h

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

try
    call plug#begin('~/.vim/plugged')
    source ~/.vim/plugins.vim
    call plug#end()
catch
endtry


"""""""""""""""""""""""""
" Plugins configuration "
"""""""""""""""""""""""""

" (built-in netrw)
let netrw_banner=0


" appearances
try
    colorscheme spaceway
catch
    colorscheme desert
endtry

let g:airline_theme='ravenpower'
if len(getbufinfo({'buflisted':1})) > 1
    let g:airline#extensions#tabline#enabled = 1
else
    let g:airline#extensions#tabline#enabled = 0
endif
let g:airline#extensions#tabline#fnamemod = ':t'

" Rooter
let g:rooter_patterns = ['.terraform', '.cloud', 'Chart.yaml']

" Powerline
try
    let g:airline_section_z = airline#section#create(["\uE0A1 " . '%{line(".")}' . " \uE0A3 " . '%{col(".")}'])
catch
endtry


" ALE
" Write this in your vimrc file
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
let g:airline#extensions#ale#enabled = 0
let g:ale_linters = {
            \   'python': ['flake8'],
            \}

" Startify
let g:startify_custom_header = vim_logo
let g:startify_bookmarks = ["~/.vimrc", "~/.xmonad/lib/"]
let g:startify_lists = [
            \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
            \ { 'type': 'dir',       'header': ['   MRU '. getcwd()] },
            \ { 'type': 'sessions',  'header': ['   Sessions']       },
            \ ]


" Terraform
let g:terraform_fmt_on_save=1

" NerdTree
let NERDTreeQuitOnOpen=1
let NERDTreeShowHidden=1
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * execute NERDTreeDirectoryAction()

" editorconfig
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

" markdown-preview
let g:mkdp_auto_start = 0
let g:mkdp_auto_close = 1

function OpenMarkdownPreview (url)
    execute "silent ! google-chrome-stable --class=Preview --new-window --app=" . a:url . " &"
endfunction
let g:mkdp_browserfunc = 'OpenMarkdownPreview'

let g:mkdp_markdown_css = ''

let g:mkdp_port = '9999'
let g:mkdp_page_title = 'Markdown Preview'
let g:mkdp_filetypes = ['markdown']
let g:mkdp_theme = 'dark'


""""""""""""""""
" Key Bindings "
""""""""""""""""

" Disable 's' command and use 'cl' instead
nmap s <Nop>
xmap s <Nop>

let mapleader = ' '
nnoremap <silent><leader>n :nohl<CR>
nnoremap <silent><leader>$ mz:%s/\s\+$//<CR>:nohl<CR>`zz
nnoremap <leader>u :UndotreeToggle<CR>
nnoremap cc <cmd>cclose<CR>

nnoremap <silent><leader>o :only<CR>

nnoremap <silent><leader>vs vip:sort<CR>

" git related bindings
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gs :Gstatus<CR>

" handle indent text object
onoremap <silent>ai :<C-U>cal <SID>IndTxtObj(0)<CR>
onoremap <silent>ii :<C-U>cal <SID>IndTxtObj(1)<CR>
vnoremap <silent>ai :<C-U>cal <SID>IndTxtObj(0)<CR><Esc>gv
vnoremap <silent>ii :<C-U>cal <SID>IndTxtObj(1)<CR><Esc>gv

" Move selection
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" Move around errors
nnoremap <silent> <C-k> <Plug>(ale_previous_wrap)
nnoremap <silent> <C-j> <Plug>(ale_next_wrap)

" NerdTree
" nnoremap <silent> <leader><space> :NERDTreeCWD<CR>
nmap <silent> <leader><space> :call ToggleTree()<CR>

" Mergetool
nnoremap <silent> <leader>md :MergetoolStop<CR>
nnoremap <silent> <leader>mg :diffg<CR>
nnoremap <silent> <leader>mp :diffp<CR>

" Add borders on # based comments
nnoremap <silent> <leader>d A<space>#<esc>yyP:s/./#/g<CR>jp:s/./#/g<CR>:nohl<CR>

" Markdown preview
nnoremap <silent> <leader>p <Plug>MarkdownPreviewToggle


" LSP

" nnoremap <silent> <C-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
" nnoremap <silent> <leader>ca <cmd>lua vim.lsp.buf.code_action()<CR>
" nnoremap <silent> <leader>e <cmd>lua vim.diagnostic.open_float()<CR>
" nnoremap <silent> <leader>wa <cmd>lua vim.lsp.buf.add_workspace_folder()<CR>
" nnoremap <silent> <leader>wl <cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>
" nnoremap <silent> <leader>wr <cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>
" nnoremap <silent> K <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> <leader>D <cmd>lua vim.lsp.buf.type_definition()<CR>
nnoremap <silent> <leader>F <cmd>lua vim.lsp.buf.formatting()<CR>
nnoremap <silent> <leader>rn <cmd>lua vim.lsp.buf.rename()<CR>
nnoremap <silent> [d <cmd>lua vim.diagnostic.goto_prev()<CR>
nnoremap <silent> ]d <cmd>lua vim.diagnostic.goto_next()<CR>
nnoremap <silent> gD <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> gi <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> gq <cmd>lua vim.diagnostic.setqflist()<CR>
nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>

" Telescope
nnoremap <silent> <leader>gg        <cmd>Telescope git_files<cr>
nnoremap <silent> <leader>ff        <cmd>Telescope find_files<CR>
nnoremap <silent> <leader>tb        <cmd>Telescope buffers<cr>
nnoremap <silent> <leader>tg        <cmd>Telescope live_grep<cr>
nnoremap <silent> <leader>tq        <cmd>Telescope quickfix<cr>
nnoremap <silent> <leader>td        <cmd>Telescope diagnostics<cr>
nnoremap <silent> <leader>tm        <cmd>Telescope marks<cr>
nnoremap <silent> <leader>tj        <cmd>Telescope jumplist<cr>
nnoremap <silent> <leader>tr        <cmd>Telescope registers<cr>

" Base64
let g:vim_base64_disable_default_key_mappings=1
vnoremap <silent> <leader>bd :<c-u>call base64#v_atob()<cr><esc>
vnoremap <silent> <leader>be :<c-u>call base64#v_btoa()<cr><esc>

"""""""""""""
" Functions "
"""""""""""""

" Start in $PROJECT_ROOT if variable exists
if $PROJECT_ROOT != ""
    cd $PROJECT_ROOT
endif

" Fix xmonad lib loction
if (match ('xmonad', expand('%:p:h') > 0))
    let ale_haskell_ghc_options='-fno-code -v0 -i ~/.xmonad/lib/'
endif

" Add support for inner code indent
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

" Open NERDTree in the right location
" Check if NERDTree is open or active
function! IsNERDTreeOpen()
    return exists("t:NERDTreeBufName") && (bufwinnr(t:NERDTreeBufName) != -1)
endfunction

function! CheckIfCurrentBufferIsFile()
    return strlen(expand('%')) > 0
endfunction

" Call NERDTreeFind iff NERDTree is active, current window contains a modifiable
" file, and we're not in vimdiff
function! SyncTree()
    if &modifiable && IsNERDTreeOpen() && CheckIfCurrentBufferIsFile() && !&diff
        NERDTreeFind
        wincmd p
    endif
endfunction

function NERDTreeDirectoryAction ()
    if argc() == 1 && isdirectory(argv()[0]) && !exists('s:std_in')
        enew
        execute 'Startify'
        execute 'NERDTreeCWD'
    endif
endfunction


" Highlight currently open buffer in NERDTree
autocmd BufRead * call SyncTree()

function! ToggleTree()
    if CheckIfCurrentBufferIsFile()
        if IsNERDTreeOpen()
            NERDTreeClose
        else
            NERDTreeFind
        endif
    else
        NERDTree
    endif
endfunction

" Git mergetool
function s:set_mergetool_layout(split)
    setlocal noundofile
    setlocal norelativenumber
    setlocal nonumber
endfunction

let g:MergetoolSetLayoutCallback = function('s:set_mergetool_layout')
