-- This fils contains everything that should be tranformed in lua
-- Once it's done, put everything in its place (keybindings, plugin config...)
-- Only keep vim defaults (flags) here

-- Set highlight on search
vim.o.hlsearch = false

-- Enable mouse mode
vim.o.mouse = 'a'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

-- Set colorscheme
vim.o.termguicolors = true
local colorscheme_loaded,_ = pcall(vim.cmd, "colorscheme spaceway")
if not colorscheme_loaded
then
	vim.cmd("colorscheme desert")
end

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- Set line numbering
vim.wo.number = true
vim.wo.relativenumber = true

-- Disable netrw
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- Disable mode info since it's shown in status line
vim.opt.showmode = false

-- highlight markdown code blocks
vim.g.markdown_fenced_languages = {'bash', 'zsh', 'sh', 'hcl', 'yaml', 'json=javascript'}

-- Configure persistence
vim.opt.backup = false
vim.opt.swapfile = false
vim.opt.undofile = true
vim.opt.undodir = os.getenv("HOME") .. '/.vim/undo'

vim.opt.splitright = true
vim.opt.spelllang="fr"
-- set hidden -- needs more search about what abandonning a buffer means

-- set termguicolors to enable highlight groups
vim.opt.termguicolors = true

-- Terraform
vim.g.terraform_fmt_on_save = true


-- """"""""""""""""
-- " Key Bindings "
-- """"""""""""""""

-- " NvimTree
vim.keymap.set('n', '<leader><space>', ':NvimTreeToggle<CR>', { desc = 'Toggle NvimTree' })

vim.keymap.set('n', '<leader>u', ':UndotreeToggle<CR>', { desc = 'Toggle UndoTree'})

vim.keymap.set('n', '<silent><leader>vs', 'vip:sort<CR>', { desc = '' })

-- " git related bindings
-- nnoremap <leader>gd :Gdiff<CR>
-- nnoremap <leader>gb :Gblame<CR>
-- nnoremap <leader>gs :Gstatus<CR>

-- " Move around errors
-- nnoremap <silent> <C-k> <Plug>(ale_previous_wrap)
-- nnoremap <silent> <C-j> <Plug>(ale_next_wrap)

-- " Mergetool
-- nnoremap <silent> <leader>md :MergetoolStop<CR>
-- nnoremap <silent> <leader>mg :diffg<CR>
-- nnoremap <silent> <leader>mp :diffp<CR>

-- " Add borders on # based comments
-- nnoremap <silent> <leader>d A<space>#<esc>yyP:s/./#/g<CR>jp:s/./#/g<CR>:nohl<CR>

-- " Base64
-- let g:vim_base64_disable_default_key_mappings=1
-- vnoremap <silent> <leader>bd :<c-u>call base64#v_atob()<cr><esc>
-- vnoremap <silent> <leader>be :<c-u>call base64#v_btoa()<cr><esc>


-- " if exists("FindRootDirectory")
-- "     autocmd VimEnter * let &tags=FindRootDirectory() . "/.tags"
-- " endif

-- " Startup
-- autocmd BufEnter * lcd %:p:h

-- " Pasting options (to be tested)
-- set pastetoggle=<F2>

-- " set path and wildmenu to find all files under cwd
-- set path+=**
-- set wildmenu

-- " appearances
-- try
--     colorscheme spaceway
-- catch
--     colorscheme desert
-- endtry

-- " Rooter
-- let g:rooter_patterns = ['.terraform', '.cloud', 'Chart.yaml']

-- " editorconfig
-- let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

-- """""""""""""
-- " Functions "
-- """""""""""""

-- " Start in $PROJECT_ROOT if variable exists
-- if $PROJECT_ROOT != ""
--     cd $PROJECT_ROOT
-- endif

-- " Fix xmonad lib loction
-- if (match ('xmonad', expand('%:p:h') > 0))
--     let ale_haskell_ghc_options='-fno-code -v0 -i ~/.xmonad/lib/'
-- endif

-- " Git mergetool
-- function s:set_mergetool_layout(split)
--     setlocal noundofile
--     setlocal norelativenumber
--     setlocal nonumber
-- endfunction

-- let g:MergetoolSetLayoutCallback = function('s:set_mergetool_layout')
