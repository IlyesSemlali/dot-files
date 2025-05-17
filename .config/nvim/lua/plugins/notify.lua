return {
	{
		"rcarriga/nvim-notify",
		opts = function()
			vim.notify = require("notify")
			require("notify").setup({
				background_colour = "#000000",
				level = 1,
				minimum_width = 20,
				render = "wrapped-compact",
				stages = "no_animation",
				time_formats = {
					notification = "%T",
					notification_history = "%FT%T",
				},
				timeout = 5000,
				top_down = true,
			})
		end,
	},
}
