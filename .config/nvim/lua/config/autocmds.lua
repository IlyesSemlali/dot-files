local autocmd = vim.api.nvim_create_autocmd

-- Auto resize panes when resizing nvim window
-- autocmd("VimResized", {
--   pattern = "*",
--   command = "tabdo wincmd =",
-- })

-- Open help pages in new tabs
autocmd({ "BufEnter" }, {
	pattern = { "*.txt" },
	callback = function()
		if vim.bo.filetype == "help" then
			vim.cmd("wincmd T")
		end
	end,
})

-- Close Neotest and DAP windows when they are the last ones remaining
vim.api.nvim_create_autocmd({ "QuitPre" }, {
	callback = function()
		local invalid_ft = {
			"neotest-summary",
			"neotest-output-panel",
			"dapui_scopes",
			"dapui_breakpoints",
			"dapui_stacks",
			"dapui_watches",
			"dapui_console",
			"dap-repl",
		}

		local wins = vim.api.nvim_list_wins()
		-- If there is more than one window, we check if they are all "sidebar" types
		if #wins > 1 then
			for _, win in ipairs(wins) do
				local buf = vim.api.nvim_win_get_buf(win)
				local ft = vim.bo[buf].filetype

				-- If the window we are looking at is in our list, close it
				if vim.tbl_contains(invalid_ft, ft) then
					vim.api.nvim_win_close(win, true)
				end
			end
		end
	end,
})
