return {
	"MeanderingProgrammer/render-markdown.nvim",
	dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-tree/nvim-web-devicons" },
	opts = {
		anti_conceal = {
			enabled = false,
		},
		heading = {
			width = "block",
			left_pad = 2,
			right_pad = 2,
			min_width = 50,
		},
	},
}
