-- These two plugins install mason (a package manager for LSP and formatters)
-- formatters are installed in the null-ls plugin (~/.config/nvim/lua/plugins/null-ls.lua),
-- and configured in conform (~/.config/nvim/lua/plugins/conform.lua)

return {
	"mason-org/mason-lspconfig.nvim",
	opts = {},
	dependencies = {
		{ "mason-org/mason.nvim", opts = {
			ui = {
				border = "rounded",
			},
		} },
		"neovim/nvim-lspconfig",
	},
}
