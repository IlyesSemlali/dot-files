return {
	"nvim-tree/nvim-tree.lua",
	version = "*",
	lazy = false,
	dependencies = {
		"nvim-tree/nvim-web-devicons",
	},
	config = function()
		require("nvim-tree").setup({
			update_cwd = true,

			git = {
				enable = true,
				ignore = false,
			},

			sort = {
				sorter = "case_sensitive",
			},

			view = {
				width = 30,
			},

			renderer = {
				group_empty = true,
				highlight_git = false,
				icons = {
					show = {
						git = true,
					},
				},
			},

			-- filters = {
			--   dotfiles = true,
			-- },
		})

		local map = vim.keymap.set
		map("n", "<leader><space>", "<cmd> NvimTreeToggle <CR>", { desc = "Toggle NvimTree" })

		-- Automatically close NvimTree on quit
		-- from https://github.com/nvim-tree/nvim-tree.lua/wiki/Auto-Close
		vim.api.nvim_create_autocmd("QuitPre", {
			command = "NvimTreeClose",
		})

		-- find more here: https://github.com/nvim-tree/nvim-tree.lua/blob/master/doc/nvim-tree-lua.txt#L2490
		vim.api.nvim_set_hl(0, "NvimTreeRootFolder", { fg = "orange" })
		vim.api.nvim_set_hl(0, "NvimTreeFolderName", { fg = "#458588" })
		vim.api.nvim_set_hl(0, "NvimTreeEmptyFolderName", { fg = "#458588" })
		vim.api.nvim_set_hl(0, "NvimTreeOpenedFolderName", { bold = true, fg = "#458588" })
	end,
}
