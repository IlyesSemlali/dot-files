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

			map("n", "<leader><space>", ":Oil --float<CR>")

			require("oil").setup({
				keymaps = {
					-- ["g?"] = { "actions.show_help", mode = "n" },
					-- ["<CR>"] = "actions.select",
					-- ["<C-s>"] = { "actions.select", opts = { vertical = true } },
					-- ["<C-h>"] = { "actions.select", opts = { horizontal = true } },
					-- ["<C-t>"] = { "actions.select", opts = { tab = true } },
					-- ["<C-p>"] = "actions.preview",
					-- ["<C-c>"] = { "actions.close", mode = "n" },
					-- ["<C-l>"] = "actions.refresh",
					["-"] = { "actions.parent", mode = "n" },
					-- ["_"] = { "actions.open_cwd", mode = "n" },
					-- ["`"] = { "actions.cd", mode = "n" },
					-- ["~"] = { "actions.cd", opts = { scope = "tab" }, mode = "n" },
					-- ["gs"] = { "actions.change_sort", mode = "n" },
					-- ["gx"] = "actions.open_external",
					-- ["g."] = { "actions.toggle_hidden", mode = "n" },
					-- ["g\\"] = { "actions.toggle_trash", mode = "n" },
				},
				float = {
					-- Padding around the floating window
					padding = 10,
					-- max_width and max_height can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
					max_width = 50,
					max_height = 30,
					border = "rounded",
					win_options = {
						winblend = 0,
					},
					-- optionally override the oil buffers window title with custom function: fun(winid: integer): string
					get_win_title = nil,
					-- preview_split: Split direction: "auto", "left", "right", "above", "below".
					preview_split = "auto",
					-- This is the config that will be passed to nvim_open_win.
					-- Change values here to customize the layout
					override = function(conf)
						return conf
					end,
				},
			})
		end,
	},
}
