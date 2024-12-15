vim.o.signcolumn = "yes"
-- To use the number column for diagnostics and git status
-- vim.o.signcolumn = "number"

local diagnostic_config = {
	signs = {
		text = {
			[vim.diagnostic.severity.ERROR] = "",
			[vim.diagnostic.severity.WARN] = "",
			[vim.diagnostic.severity.HINT] = "󱡁",
			[vim.diagnostic.severity.INFO] = "",
		},
	},
}

vim.diagnostic.config(diagnostic_config)
