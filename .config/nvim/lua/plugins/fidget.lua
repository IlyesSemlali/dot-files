return {
	{
		"j-hui/fidget.nvim",
		tag = "v1.6.1", -- Make sure to update this to something recent!
		opts = {
			notification = {
				override_vim_notify = true, -- 🚩 this makes Fidget call vim.notify(...)
				redirect = function(msg, level, opts)
					local adapted_timeout = string.len(msg) * 10 + 800
					local opts = { timeout = adapted_timeout }
					return require("fidget.integration.nvim-notify").delegate(msg, level, opts)
				end,
			},
			progress = {
				ignore = {
					"ltex",
				},
			},
		},
	},
}
