return {
  {
    "mbbill/undotree",
    lazy = false,
    config = function()
      local map = vim.keymap.set

      map("n", "<leader>u", "<cmd> UndotreeToggle <CR>", { desc = "Open UndoTree" })
    end,
  },
}
