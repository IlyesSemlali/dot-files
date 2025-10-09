-- Gitsigns adds git signs in the number column, and more importantly
-- adds git related commands such as: diff, stage_hunk, blame line...
return {
	{
		"lewis6991/gitsigns.nvim",
		lazy = false,
		config = function()
			local map = vim.keymap.set

			local gitsigns = require("gitsigns")
			-- local function map(mode, l, r, opts)
			-- 	opts = opts or {}
			-- 	vim.keymap.set(mode, l, r, opts)
			-- end

			gitsigns.setup({
				sign_priority = 80,
			})

			map("n", "<leader>gS", gitsigns.stage_buffer, { desc = "Git: stage buffer" })
			map("n", "<leader>gs", gitsigns.stage_hunk, { desc = "Git: stage hunk" })
			map("v", "<leader>gs", function()
				gitsigns.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
			end, { desc = "Git: stage hunk" })

			map("n", "<leader>gR", gitsigns.reset_buffer, { desc = "Git: reset buffer" })
			map("n", "<leader>gr", gitsigns.reset_hunk, { desc = "Git: reset hunk" })
			map("v", "<leader>gr", function()
				gitsigns.reset_hunk({ vim.fn.line("."), vim.fn.line("v") })
			end, { desc = "Git: reset hunk" })

			map("n", "<leader>gp", gitsigns.preview_hunk_inline, { desc = "Git: preview hunk" })

			map("n", "<C-p>", function()
				gitsigns.nav_hunk("prev")
			end, { desc = "Git: previous hunk" })

			map("n", "<C-n>", function()
				gitsigns.nav_hunk("next")
			end, { desc = "Git: next hunk" })

			map("n", "<leader>gb", function()
				gitsigns.blame_line({ full = true })
			end, { desc = "Git: blame" })

			map("n", "<leader>gd", function()
				gitsigns.diffthis("~")
			end, { desc = "Git: diff" })
		end,
	},
}
