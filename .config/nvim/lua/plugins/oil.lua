-- This is the file manager, can be used as a project
-- tree, but it's supposably best to use Telescope for
-- file navigation
--
-- TODO: Set highlight colors to match eza
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
					["<Esc>"] = { "actions.close", mode = "n" },
					["-"] = { "actions.parent", mode = "n" },
					["_"] = { "actions.open_cwd", mode = "n" },
					["<C-.>"] = { "actions.toggle_hidden", mode = "n" },
				},
				float = {
					-- Padding around the floating window
					padding = 4,
					-- max_width and max_height can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
					max_width = 60,
					max_height = 25,
					border = "rounded",
					preview_split = "right",
					-- This is the config that will be passed to nvim_open_win.
					-- Change values here to customize the layout
					override = function(conf)
						return conf
					end,
				},
			})

			map("n", "-", function()
				require("oil").open_float()
			end, { silent = true, desc = "Oil: Open file's parent directory" })

			map("n", "_", function()
				require("oil").open_float(vim.fn.getcwd())
			end, { silent = true, desc = "Oil: Open working directory" })
		end,
	},
}
