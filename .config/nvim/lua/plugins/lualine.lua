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
					lualine_c = {
						"location",

						-- Show the YAML key when in a YAML file
						function()
							local loaded, yaml_nvim = pcall(require, "yaml_nvim")
							if loaded then
								return yaml_nvim.get_yaml_key()
							else
								return ""
							end
						end,
					},

					lualine_x = { "progress" },
					lualine_y = { "diff" },
					lualine_z = { "branch" },
				},
			})
		end,
	},
}
