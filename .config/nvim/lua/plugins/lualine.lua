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
				sections = {
					lualine_a = { "mode" },
					lualine_b = { "filetype", "filename" },
					lualine_c = { "location" },
					-- TODO: use a function to disable YAML key when not in a YAML file
					-- lualine_c = { "location", require("yaml_nvim").get_yaml_key_and_value },

					lualine_x = { "progress" },
					lualine_y = { "diff" },
					lualine_z = { "branch" },
				},
			})
		end,
	},
}
