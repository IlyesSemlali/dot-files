return {
  {
    "rhysd/conflict-marker.vim",
    lazy = false,
    config = function()
      local global = vim.g

      -- disable the default highlight group
      global.conflict_marker_highlight_group = ""

      -- Include text after begin and end markers
      global.conflict_marker_begin = "^<<<<<<<+ .*$"
      global.conflict_marker_common_ancestors = "^|||||||+ .*$"
      global.conflict_marker_end = "^>>>>>>>+ .*$"

      vim.api.nvim_set_hl(0, "ConflictMarkerBegin", { bg = "#2f7366" })
      vim.api.nvim_set_hl(0, "ConflictMarkerOurs", { bg = "#2e5049" })
      vim.api.nvim_set_hl(0, "ConflictMarkerTheirs", { bg = "#344f69" })
      vim.api.nvim_set_hl(0, "ConflictMarkerEnd", { bg = "#2f628e" })
      vim.api.nvim_set_hl(0, "ConflictMarkerCommonAncestorsHunk", { bg = "#754a81" })
    end,
  },
}
