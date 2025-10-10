-- The null-ls is a complement to Mason and allows to install automatically
-- formatters (which Mason cannot do on its own)
return {
	{
		"jay-babu/mason-null-ls.nvim",
		event = { "BufReadPre", "BufNewFile" },
		dependencies = {
			"williamboman/mason.nvim",
			"nvimtools/none-ls.nvim",
		},
		config = function()
			local mason_null_ls = require("mason-null-ls")

			mason_null_ls.setup({
				automatic_installation = true,
				ensure_installed = {
					"black",
					"jq",
					"python-lsp-server",
					"ruff",
					"stylua",
					"taplo",
					"terraform_fmt",
					"yaml-language-server",
					"yamlfmt",
				},
			})
		end,
	},
}
