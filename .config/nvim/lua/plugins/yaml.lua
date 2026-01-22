return {
	{
		"https://tangled.org/cuducos.me/yaml.nvim",

		ft = { "yaml" },

		dependencies = {
			"nvim-telescope/telescope.nvim",
		},
		config = function()
			local map = vim.keymap.set
			yaml = require("yaml_nvim")

			map("n", "<leader>fy", function()
				yaml.telescope()
			end, { desc = "Telescope: Search YAML" })

			map("n", "<leader>yy", function()
				yaml.yank_value()
				print("yanked YAML value")
			end, { desc = "YAML: Yank Value" })

			map("n", "<leader>YY", function()
				yaml.yank_key()
				print("yanked YAML key")
			end, { desc = "YAML: Yank Key" })

			--
		end,
	},
}
