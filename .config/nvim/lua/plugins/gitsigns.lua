return {
	{
		"lewis6991/gitsigns.nvim",
		lazy = false,
		config = function()
			-- local map = vim.keymap.set

			local gitsigns = require("gitsigns")
			local function map(mode, l, r, opts)
				opts = opts or {}
				vim.keymap.set(mode, l, r, opts)
			end

			gitsigns.setup()

			-- TODO: add descriptions to all these new mappings
			map("n", "<leader>gs", gitsigns.stage_hunk)
			map("v", "<leader>gs", function()
				gitsigns.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
			end)

			map("n", "<leader>gr", gitsigns.reset_hunk)
			map("v", "<leader>gr", function()
				gitsigns.reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
			end)

			map("n", "<leader>gu", gitsigns.undo_stage_hunk)
			map("n", "<leader>gp", gitsigns.preview_hunk_inline)

			map("n", "<leader>gS", gitsigns.stage_buffer)
			map("n", "<leader>gR", gitsigns.reset_buffer)

			map("n", "<leader>gb", function()
				gitsigns.blame_line({ full = true })
			end)

			-- map("n", "<leader>tb", gitsigns.toggle_current_line_blame)
			map("n", "<leader>gd", gitsigns.diffthis)

			map("n", "<leader>gD", function()
				gitsigns.diffthis("~")
			end)
		end,
	},
}
