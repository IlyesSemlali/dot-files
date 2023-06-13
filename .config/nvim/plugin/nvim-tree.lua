local function on_attach(bufnr)
  local api = require('nvim-tree.api')

  local function opts(desc)
    return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
  end


  -- Opening files
  vim.keymap.set('n', 'l', api.node.open.edit, opts('Open'))
  vim.keymap.set('n', '<CR>', api.node.open.edit, opts('Open'))
  vim.keymap.set('n', '<2-LeftMouse>', api.node.open.edit, opts('Open'))
  vim.keymap.set('n', '<C-h>', api.node.open.horizontal, opts('Open: Horizontal Split'))
  vim.keymap.set('n', '<C-v>', api.node.open.vertical, opts('Open: Vertical Split'))
  vim.keymap.set('n', '<C-t>', api.node.open.tab, opts('Open: New Tab'))

  -- File Navigation
  vim.keymap.set('n', 'L', api.tree.change_root_to_node, opts('CD'))
  vim.keymap.set('n', 'h', api.node.navigate.parent_close, opts('Close Directory'))
  vim.keymap.set('n', '<Tab>', api.node.open.preview, opts('Open Preview'))
  vim.keymap.set('n', 'I', api.node.show_info_popup, opts('Info'))
  vim.keymap.set('n', 'R', api.tree.reload, opts('Refresh'))
  vim.keymap.set('n', 'H', api.tree.change_root_to_parent, opts('Up'))

  -- File management
  vim.keymap.set('n', 'c', api.fs.create, opts('Create'))
  vim.keymap.set('n', 'D', api.fs.trash, opts('Trash'))
  vim.keymap.set('n', 'r', api.fs.rename, opts('Rename'))
  vim.keymap.set('n', '<C-r>', api.fs.rename_sub, opts('Rename: Omit Filename'))
  vim.keymap.set('n', 'd', api.fs.cut, opts('Cut'))
  vim.keymap.set('n', 'y', api.fs.copy.node, opts('Copy'))
  vim.keymap.set('n', 'p', api.fs.paste, opts('Paste'))
  vim.keymap.set('n', 'P', api.marks.bulk.move, opts('Move Bookmarked'))
  vim.keymap.set('n', 'gyp', api.fs.copy.relative_path, opts('Copy Relative Path'))
  vim.keymap.set('n', 'gya', api.fs.copy.absolute_path, opts('Copy Absolute Path'))
  vim.keymap.set('n', 'gyn', api.fs.copy.filename, opts('Copy Name'))

  vim.keymap.set('n', 'f', api.live_filter.start, opts('Filter'))

  vim.keymap.set('n', 's', api.node.run.system, opts('Run System'))

  -- Tree Navigation
  vim.keymap.set('n', 'b', api.marks.toggle, opts('Toggle Bookmark'))
  vim.keymap.set('n', '<', api.node.navigate.sibling.first, opts('First Sibling'))
  vim.keymap.set('n', '>', api.node.navigate.sibling.last, opts('Last Sibling'))
  vim.keymap.set('n', '-', api.tree.collapse_all, opts('Collapse'))

end

-- NvimTree Settings
local nvimtree_loaded, nvimtree = pcall(require, "nvim-tree")
if nvimtree_loaded
then
  nvimtree.setup({
    on_attach = on_attach,
    sort_by = "case_sensitive",
    actions = {
      open_file = {
        quit_on_open = false
      }
    },
    view = {
      adaptive_size = false,
    },
    renderer = {
      group_empty = true,
    },
    filters = {
      dotfiles = false,
    },
  })
end

-- Close nvim-tree when it's the last buffer
-- https://github.com/nvim-tree/nvim-tree.lua/issues/1005#issuecomment-1183468091
--
-- nvim-tree is also there in modified buffers so this function filter it out
local modifiedBufs = function(bufs)
    local t = 0
    for k,v in pairs(bufs) do
        if v.name:match("NvimTree_") == nil then
            t = t + 1
        end
    end
    return t
end

vim.api.nvim_create_autocmd("BufEnter", {
    nested = true,
    callback = function()
        if #vim.api.nvim_list_wins() == 1 and
        vim.api.nvim_buf_get_name(0):match("NvimTree_") ~= nil and
        modifiedBufs(vim.fn.getbufinfo({bufmodified = 1})) == 0 then
            vim.cmd "quit"
        end
    end
})
