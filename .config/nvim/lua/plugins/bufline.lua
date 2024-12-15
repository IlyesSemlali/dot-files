return {
	{
		"akinsho/bufferline.nvim",
		version = "*",
		dependencies = "nvim-tree/nvim-web-devicons",
		config = function()
			local map = vim.keymap.set

			require("bufferline").setup({
				options = {
					show_buffer_close_icons = false,
					show_close_icon = false,
					tab_size = 12,
					right_mouse_command = false,
					middle_mouse_command = false,
					always_show_bufferline = false,
					auto_toggle_bufferline = true,

					hover = {
						enabled = true,
						delay = 200,
						reveal = { "close" },
					},
				},
				highlights = {
					-- fill = {
					-- 	fg = "<colour-value-here>",
					-- 	bg = "<colour-value-here>",
					-- },
					-- background = {
					-- 	fg = "<colour-value-here>",
					-- 	bg = "<colour-value-here>",
					-- },
					-- tab = {
					-- 	fg = "<colour-value-here>",
					-- 	bg = "<colour-value-here>",
					-- },
					-- tab_selected = {
					-- 	fg = "<colour-value-here>",
					-- 	bg = "<colour-value-here>",
					-- },
					-- tab_separator = {
					-- 	fg = "<colour-value-here>",
					-- 	bg = "<colour-value-here>",
					-- },
					-- tab_separator_selected = {
					-- 	fg = "<colour-value-here>",
					-- 	bg = "<colour-value-here>",
					-- 	sp = "<colour-value-here>",
					-- 	underline = "<colour-value-here>",
					-- },
					buffer_selected = {
						bold = false,
						italic = false,
					},
					-- diagnostic = {
					-- 	fg = "<colour-value-here>",
					-- 	bg = "<colour-value-here>",
					-- },
					-- diagnostic_visible = {
					-- 	fg = "<colour-value-here>",
					-- 	bg = "<colour-value-here>",
					-- },
					-- diagnostic_selected = {
					-- 	fg = "<colour-value-here>",
					-- 	bg = "<colour-value-here>",
					-- 	bold = true,
					-- 	italic = true,
					-- },
					-- modified = {
					-- 	fg = "<colour-value-here>",
					-- 	bg = "<colour-value-here>",
					-- },
					-- modified_visible = {
					-- 	fg = "<colour-value-here>",
					-- 	bg = "<colour-value-here>",
					-- },
					-- modified_selected = {
					-- 	fg = "<colour-value-here>",
					-- 	bg = "<colour-value-here>",
					-- },
					-- separator_selected = {
					-- 	fg = "<colour-value-here>",
					-- 	bg = "<colour-value-here>",
					-- },
					-- separator_visible = {
					-- 	fg = "<colour-value-here>",
					-- 	bg = "<colour-value-here>",
					-- },
					-- separator = {
					-- 	fg = "<colour-value-here>",
					-- 	bg = "<colour-value-here>",
					-- },
				},
			})

			map("n", "<tab>", ":BufferLineCycleNext<CR>", { desc = "buffer goto next" })
			map("n", "<S-tab>", ":BufferLineCyclePrev<CR>", { desc = "buffer goto next" })
		end,
	},
}
