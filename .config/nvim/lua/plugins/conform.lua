return {
	{
		"stevearc/conform.nvim",
		lazy = false,
		config = function()
			require("conform").setup({
				log_level = vim.log.levels.DEBUG,
				format_on_save = {
					lsp_format = "fallback",
					timeout_ms = 1000,
				},
				formatters_by_ft = {
					json = { "jq" },
					lua = { "stylua" },
					python = { "ruff" },
					sh = { "shfmt" },
					terraform = { "terraform_fmt" },
					toml = { "taplo" },
					yaml = { "yamlfmt" },
					-- zsh = { "shfmt" },
					["*"] = { "trim_whitespace", "trim_newlines" },
				},
			})
			vim.api.nvim_create_autocmd("BufWritePre", {
				pattern = "*",
				callback = function(args)
					require("conform").format({ bufnr = args.buf })
				end,
			})
		end,
	},
}
