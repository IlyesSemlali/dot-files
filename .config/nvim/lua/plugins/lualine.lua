return {
	{
		"nvim-lualine/lualine.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			vim.o.showmode = false
			require("lualine").setup({
				options = {
					globalstatus = true,
				},
			})
		end,
	},
}
