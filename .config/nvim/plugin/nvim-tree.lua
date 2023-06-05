-- NvimTree Settings
local nvimtree_loaded, nvimtree = pcall(require, "nvim-tree")
if nvimtree_loaded
then
  nvimtree.setup({
    sort_by = "case_sensitive",
    actions = {
      open_file = {
        quit_on_open = true
      }
    },
    view = {
      adaptive_size = true,
      mappings = {
        list = {
          { key = "u", action = "dir_up" },
        },
      },
    },
    renderer = {
      group_empty = true,
    },
    filters = {
      dotfiles = false,
    },
  })
end

-- TODO: use left to fold and right to unfold
-- TODO: open tree when opening a folder

-- -- NerdTree
-- let NERDTreeQuitOnOpen=1
-- let NERDTreeShowHidden=1
-- autocmd StdinReadPre * let s:std_in=1
-- autocmd VimEnter * execute NERDTreeDirectoryAction()

-- " Open NERDTree in the right location
-- " Check if NERDTree is open or active
-- function! IsNERDTreeOpen()
--     return exists("t:NERDTreeBufName") && (bufwinnr(t:NERDTreeBufName) != -1)
-- endfunction

-- function! CheckIfCurrentBufferIsFile()
--     return strlen(expand('%')) > 0
-- endfunction

-- " Call NERDTreeFind iff NERDTree is active, current window contains a modifiable
-- " file, and we're not in vimdiff
-- function! SyncTree()
--     if &modifiable && IsNERDTreeOpen() && CheckIfCurrentBufferIsFile() && !&diff
--         NERDTreeFind
--         wincmd p
--     endif
-- endfunction

-- function NERDTreeDirectoryAction ()
--     if argc() == 1 && isdirectory(argv()[0]) && !exists('s:std_in')
--         enew
--         execute 'Startify'
--         execute 'NERDTreeCWD'
--     endif
-- endfunction


-- " Highlight currently open buffer in NERDTree
-- autocmd BufRead * call SyncTree()

-- function! ToggleTree()
--     if CheckIfCurrentBufferIsFile()
--         if IsNERDTreeOpen()
--             NERDTreeClose
--         else
--             NERDTreeFind
--         endif
--     else
--         NERDTree
--     endif
-- endfunction

-- " Open NERDTree in the right location
-- " Check if NERDTree is open or active
-- function! IsNERDTreeOpen()
--     return exists("t:NERDTreeBufName") && (bufwinnr(t:NERDTreeBufName) != -1)
-- endfunction

-- function! CheckIfCurrentBufferIsFile()
--     return strlen(expand('%')) > 0
-- endfunction

-- " Call NERDTreeFind iff NERDTree is active, current window contains a modifiable
-- " file, and we're not in vimdiff
-- function! SyncTree()
--     if &modifiable && IsNERDTreeOpen() && CheckIfCurrentBufferIsFile() && !&diff
--         NERDTreeFind
--         wincmd p
--     endif
-- endfunction

-- function NERDTreeDirectoryAction ()
--     if argc() == 1 && isdirectory(argv()[0]) && !exists('s:std_in')
--         enew
--         execute 'Startify'
--         execute 'NERDTreeCWD'
--     endif
-- endfunction


-- " Highlight currently open buffer in NERDTree
-- autocmd BufRead * call SyncTree()

-- function! ToggleTree()
--     if CheckIfCurrentBufferIsFile()
--         if IsNERDTreeOpen()
--             NERDTreeClose
--         else
--             NERDTreeFind
--         endif
--     else
--         NERDTree
--     endif
-- endfunction

