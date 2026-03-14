vim.api.nvim_create_autocmd("ColorScheme", {
	pattern = "*",
	callback = function()
		-- This creates highlighting groups before nvim-indent-blankline is loaded
		-- not setting them, results in an error when starting
		vim.api.nvim_set_hl(0, "IblScope", { link = "Whitespace" })
		vim.api.nvim_set_hl(0, "IblIndent", { fg = "#3b4261", bg = "NONE" })

		local transparent_groups = {
			-- Regular HL Groups
			"Normal",
			"NormalNC",
			"NormalFloat",
			"FloatBorder",

			-- Plugin Specific HL groups
			"LazyNormal",
			"LazyBorder",

			-- Gutter
			"DiagnosticSignError",
			"DiagnosticSignWarn",
			"DiagnosticSignInfo",
			"DiagnosticSignHint",
			"LineNr",
			"CursorLineNr",
			"SignColumn",
			"FoldColumn",
		}

		for _, group in ipairs(transparent_groups) do
			-- Force the background to be completely transparent
			vim.api.nvim_set_hl(0, group, { bg = "NONE", ctermbg = "NONE" })
		end
	end,
})
