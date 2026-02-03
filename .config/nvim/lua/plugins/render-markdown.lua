return {
	"MeanderingProgrammer/render-markdown.nvim",
	dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-tree/nvim-web-devicons" },
	config = function()
		require("render-markdown").setup({
			anti_conceal = {
				enabled = false,
			},
			heading = {
				width = "block",
				left_pad = 2,
				right_pad = 2,
				min_width = 50,
			},
		})

		local map = vim.keymap.set

		map("n", "<leader>rr", function()
			require("render-markdown").toggle()
		end, { silent = true, desc = "RenderMarkdown: Toggle" })
	end,
}
