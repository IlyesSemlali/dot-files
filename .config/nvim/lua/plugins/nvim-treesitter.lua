return {
	{
		"nvim-treesitter/nvim-treesitter",
		dependencies = {
			"nvim-treesitter/nvim-treesitter-textobjects",
		},
		build = ":TSUpdate",

		config = function()
			vim.treesitter.language.register("bash", "sh")

			require("nvim-treesitter").setup()

			require("nvim-treesitter").install({
				"bash",
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
				"typescript",
				"vim",
				"yaml",
			})

			vim.api.nvim_create_autocmd("FileType", {
				callback = function()
					pcall(vim.treesitter.start)
				end,
			})

			local ts_select = require("nvim-treesitter-textobjects.select")
			local keymaps = {
				["af"] = "@function.outer",
				["if"] = "@function.inner",
				["ac"] = "@class.outer",
				["ic"] = "@class.inner",
			}

			for key, query in pairs(keymaps) do
				vim.keymap.set({ "x", "o" }, key, function()
					ts_select.select_textobject(query, "textobjects")
				end, { desc = "Select " .. query })
			end
		end,
	},
}
