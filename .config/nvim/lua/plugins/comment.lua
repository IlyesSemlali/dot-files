return {
	-- Comment Context from Treesitter
	{
		"JoosepAlviste/nvim-ts-context-commentstring",
		dependencies = { "nvim-treesitter/nvim-treesitter" },
		opts = {
			enable_autocmd = false,
		},
	},

	-- Comment Plugin
	{
		"numToStr/Comment.nvim",
		dependencies = {
			"JoosepAlviste/nvim-ts-context-commentstring",
		},
		opts = function()
			-- Defers the 'require' statement until plugin loads to prevent errors
			local ts_context_integration = require("ts_context_commentstring.integrations.comment_nvim")

			return {
				pre_hook = ts_context_integration.create_pre_hook(),
			}
		end,
	},
}
