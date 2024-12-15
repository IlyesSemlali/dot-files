return {
	{
		"neovim/nvim-lspconfig",
		dependencies = {
			-- format & linting
			{
				"jose-elias-alvarez/null-ls.nvim",
			},
		},
		-- opts = require "configs.lspconfig",
		config = function()
			local map = vim.keymap.set

			map("n", "<leader>ce", vim.diagnostic.goto_next, { desc = "Diagnostics: next error" })
			map("n", "<leader>cE", vim.diagnostic.goto_prev, { desc = "Diagnostics: previous error" })
			map("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Diagnostics: set loclist" })
			map("n", "<leader>cd", vim.lsp.buf.definition, { desc = "LSP: Go to definition" })
			map("n", "<leader>cr", vim.lsp.buf.rename, { desc = "LSP: Rename" })
		end,
	},
}
