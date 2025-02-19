return {
	{

		"nvim-treesitter/nvim-treesitter",
		dependencies = {
			"nvim-treesitter/nvim-treesitter-textobjects",
		},
		build = ":TSUpdate",

		config = function()
			require("nvim-treesitter.configs").setup({
				ensure_installed = {
					"c",
					"css",
					"html",
					"javascript",
					"lua",
					"markdown",
					"markdown_inline",
					"python",
					"tsx",
					"typescript",
					"vim",
				},

				textobjects = {
					select = {
						enable = true,
						lookahead = true,
						keymaps = {
							["af"] = "@function.outer",
							["if"] = "@function.inner",
							["ac"] = "@class.outer",
							["ic"] = "@class.inner",
						},
					},
				},

				indent = {
					enable = true,
					-- disable = {
					--   "python"
					-- },
				},
			})
		end,
	},
}
