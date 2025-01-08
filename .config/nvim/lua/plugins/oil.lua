-- This is the file manager, can be used as a project
-- tree, but it's supposably best to use Telescope for
-- file navigation
return {
	{
		"stevearc/oil.nvim",
		---@module 'oil'
		---@type oil.SetupOpts
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			local map = vim.keymap.set

			require("oil").setup({
				keymaps = {
					-- ["g?"] = { "actions.show_help", mode = "n" },
					["<CR>"] = { "actions.select", opts = { close = true } },
					["<C-v>"] = { "actions.select", opts = { vertical = true } },
					["<C-h>"] = { "actions.select", opts = { horizontal = true } },
					["<C-t>"] = { "actions.select", opts = { tab = true } },
					["<C-p>"] = "actions.preview",
					["<Leader><Space>"] = { "actions.close", mode = "n" },
					["-"] = { "actions.parent", mode = "n" },
					["_"] = { "actions.open_cwd", mode = "n" },
					["<C-.>"] = { "actions.toggle_hidden", mode = "n" },
				},
				float = {
					-- Padding around the floating window
					padding = 0,
					-- max_width and max_height can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
					max_width = 100,
					max_height = 40,
					border = "rounded",
					preview_split = "right",
					-- This is the config that will be passed to nvim_open_win.
					-- Change values here to customize the layout
					override = function(conf)
						return conf
					end,
				},
			})

			map("n", "<leader><space>", function()
				require("oil").open_float()
			end, { silent = true })
		end,
	},
}
