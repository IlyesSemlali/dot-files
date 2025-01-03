return {
	{
		"xiyaowong/transparent.nvim",
		lazy = false,
		opts = {
			extra_groups = {
				"NormalFloat", -- plugins which have float panel such as Lazy, Mason, LspInfo
				"NvimTreeNormal", -- NvimTree
				"DiagnosticSignError",
				"DiagnosticSignWarn",
				"DiagnosticSignInfo",
				"DiagnosticSignHint",
			},
		},
    config = function ()
      vim.cmd "TransparentEnable"
    end
	},
}
