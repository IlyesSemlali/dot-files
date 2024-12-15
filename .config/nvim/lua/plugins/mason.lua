-- These two plugins install mason (a package manager for LSP and formatters)
-- formatters are installed in the null-ls plugin, and configured in conform
return {
	{
		"williamboman/mason.nvim",
	},
	{
		"williamboman/mason-lspconfig.nvim",
		opts = function()
			require("mason").setup()
			require("mason-lspconfig").setup({

				-- List of available lspconfigs are present here:
				-- https://github.com/williamboman/mason-lspconfig.nvim?tab=readme-ov-file#available-lsp-servers
				ensure_installed = {
					-- lua
					"lua_ls",

					-- Text
					"ltex",

					-- bash
					"bashls",

					-- python
					"pyright",
					"pylsp",

					-- Ansible
					"ansiblels",

					-- Kubernetes
					"yamlls",
					"helm_ls",

					-- Terraform
					"terraformls",
				},
			})
		end,
	},
}
