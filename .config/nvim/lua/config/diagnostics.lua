local diagnostic_config = {
	signs = {
		text = {
			[vim.diagnostic.severity.ERROR] = "",
			[vim.diagnostic.severity.WARN] = "",
			[vim.diagnostic.severity.HINT] = "󱡁",
			[vim.diagnostic.severity.INFO] = "",
		},
		priority = 20,
	},
}

vim.diagnostic.config(diagnostic_config)
