return {
	{
		"nvim-lualine/lualine.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			vim.o.showmode = false
			require("lualine").setup({
				options = {
					globalstatus = true,
					sections = {
						lualine_x = { require("yaml_nvim").get_yaml_key_and_value },
					},
				},
			})
		end,
	},
}
