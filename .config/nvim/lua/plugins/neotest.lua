return {
	{
		"nvim-neotest/neotest",
		dependencies = {
			"nvim-neotest/nvim-nio",
			"nvim-lua/plenary.nvim",
			"antoinemadec/FixCursorHold.nvim",
			"nvim-treesitter/nvim-treesitter",

			-- Adapters
			"nvim-neotest/neotest-python",
		},
		config = function()
			require("neotest").setup({
				floating = {
					border = "rounded",
				},
				adapters = {
					require("neotest-python")({
						runner = "pytest",
					}),
				},
			})
		end,
		keys = {
			{
				"<leader>tr",
				function()
					require("neotest").run.run()
				end,
				desc = "Tests: Run nearest test",
			},
			{
				"<leader>tf",
				function()
					require("neotest").run.run(vim.fn.expand("%"))
				end,
				desc = "Tests: Run file",
			},
			{
				"<leader>tt",
				function()
					require("neotest").summary.toggle()
				end,
				desc = "Tests: toggle summary",
			},
			{
				"<leader>to",
				function()
					require("neotest").output.open({ enter = true })
				end,
				desc = "Tests: show output",
			},
		},
	},
}
