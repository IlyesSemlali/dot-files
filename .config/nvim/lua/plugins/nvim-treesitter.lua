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
					"hcl",
					"html",
					"javascript",
					"jinja",
					"jinja_inline",
					"lua",
					"markdown",
					"markdown_inline",
					"nix",
					"python",
					"tsx",
					"terraform",
					"typescript",
					"vim",
				},

				highlight = {
					enable = true,
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
					enable = false,
				},
			})
		end,
	},
}
