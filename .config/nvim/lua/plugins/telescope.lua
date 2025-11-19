return {
	{
		"nvim-telescope/telescope.nvim",
		branch = "0.1.x",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope-fzf-native.nvim",
			"nvim-telescope/telescope-ui-select.nvim",
		},

		config = function()
			-- Telescope
			local map = vim.keymap.set
			local telescope = require("telescope")
			telescope.setup({

				defaults = {
					path_display = { "smart" },
					scroll_strategy = "limit",

					layout_config = {

						height = 0.9,
						width = 0.9,

						bottom_pane = {
							height = 0.33,
							preview_width = 0.6,
						},

						center = {
							height = 0.6,
							preview_cutoff = 40,
							preview_width = 0.6,
							width = 0.5,
						},

						cursor = {
							height = 5,
							width = 10,
						},

						horizontal = {
							preview_cutoff = 120,
							preview_width = 0.6,
							padding = 5,
						},

						vertical = {
							height = 0.7,
							width = 0.5,
							preview_height = 0.6,
						},
					},
				},

				pickers = {

					buffers = {
						layout_strategy = "bottom_pane",
						preview = false,
					},

					oldfiles = {
						layout_strategy = "vertical",
					},

					lsp_references = {
						layout_strategy = "bottom_pane",
					},

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

				extensions = {
					["ui-select"] = {
						layout_strategy = "cursor",

						layout_config = {
							cursor = {
								layout_strategy = "vertical",
								prompt_position = "top",
								width = 0.35,
								height = 0.30,
							},
						},
						previewer = false,
					},
				},
			})

			local builtin = require("telescope.builtin")
			require("telescope").load_extension("ui-select")

			map("n", "<leader><space>", builtin.buffers, { desc = "Telescope: Buffers" })
			map("n", "<leader>ch", builtin.keymaps, { desc = "Telescope: Key Mappings" })
			map("n", "<leader>ff", builtin.find_files, { desc = "Telescope: Files" })
			map("n", "<leader>fo", builtin.oldfiles, { desc = "Telescope: Old Files" })
			map("n", "<leader>fg", builtin.live_grep, { desc = "Telescope: Live grep" })
			map("n", "<leader>fd", builtin.diagnostics, { desc = "Telescope: Diagnostics" })
			map("n", "<leader>fr", builtin.registers, { desc = "Telescope: Show Registers" })
			map("n", "<leader>fm", builtin.marks, { desc = "Telescope: Marks" })
			map("n", "<leader>cf", builtin.lsp_references, { desc = "LSP: Find references" })
		end,
	},
}
