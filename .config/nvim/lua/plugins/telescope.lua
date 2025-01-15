return {
	{
		"nvim-telescope/telescope.nvim",
		branch = "0.1.x",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope-fzf-native.nvim",
		},

		config = function()
			-- Telescope
			local map = vim.keymap.set
			local telescope = require("telescope")
			telescope.setup({

				defaults = {
					layout_config = {
						height = 0.9,
						width = 0.9,

						preview_cutoff = 20,
						preview_width = 70,
						prompt_position = "bottom",
						bottom_pane = {
							height = 25,
							preview_cutoff = 120,
							prompt_position = "top",
						},
						center = {
							height = 0.6,
							preview_cutoff = 40,
							prompt_position = "top",
							width = 0.5,
						},
						-- cursor = {
						-- 	height = 0.9,
						-- 	preview_cutoff = 40,
						-- 	width = 0.8,
						-- },
						horizontal = {
							preview_cutoff = 120,
						},
						vertical = {},
					},
				},

				pickers = {
					diagnostics = {
						layout_strategy = "bottom_pane",

						qflist_previewer = require("telescope.previewers").new_buffer_previewer({
							title = "Diagnostics",
							dyn_title = function(_, entry)
								return entry.title
							end,

							get_buffer_by_name = function(_, entry)
								return "diagnostics_" .. tostring(entry.nr)
							end,

							define_preview = function(self, entry)
								vim.api.nvim_buf_set_lines(self.state.bufnr, 0, -1, false, { entry.text })
							end,
						}),
					},
				},
				extensions = {},
			})

			local builtin = require("telescope.builtin")
			map("n", "<leader>ch", builtin.keymaps, { desc = "Telescope: Key Mappings" })
			map("n", "<leader>fo", builtin.oldfiles, { desc = "Telescope: Old Files" })
			map("n", "<leader>ff", builtin.find_files, { desc = "Telescope: Find all" })
			map("n", "<leader>fg", builtin.live_grep, { desc = "Telescope: Live grep" })
			map("n", "<leader>fd", builtin.diagnostics, { desc = "Telescope: Diagnostics" })
			map("n", "<leader>fr", builtin.registers, { desc = "Telescope: Show Registers" })
		end,
	},
}
