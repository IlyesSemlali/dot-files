return {
	"aserowy/tmux.nvim",
	config = function()
		require("tmux").setup({
			navigation = {
				cycle_navigation = false,
				enable_default_keybindings = true,
			},
		})

		local map = vim.keymap.set
		local tmux = require("tmux")

		map("n", "<C-S-h>", function()
			tmux.move_left()
		end, { desc = "Navigation: Left" })

		map("n", "<C-S-l>", function()
			tmux.move_right()
		end, { desc = "Navigation: Right" })

		map("n", "<C-S-j>", function()
			tmux.move_bottom()
		end, { desc = "Navigation: Bottom" })

		map("n", "<C-S-k>", function()
			tmux.move_top()
		end, { desc = "Navigation: Up" })
	end,
}
