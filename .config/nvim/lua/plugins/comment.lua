return {
	"numToStr/Comment.nvim",
	opts = function()
		return {
			pre_hook = function(ctx)
				-- Determine whether to use line or block commentstring
				local type = ctx.ctype == require("Comment.utils").ctype.blockwise and "block" or "line"

				-- Use Neovim's native 0.10+ commentstring detection API
				return vim.filetype.get_option(vim.bo.filetype, "commentstring")
			end,
		}
	end,
}
