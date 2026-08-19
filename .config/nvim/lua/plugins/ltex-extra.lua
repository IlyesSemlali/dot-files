-- Adds "Add to dictionary" / "disable rule" / "hide false positive" code
-- actions for ltex (trigger with the <leader>ca mapping in mappings.lua).
-- Word lists live as plain text files (one word per line) under
-- ~/.config/nvim/spell/ltex/, named ltex.dictionary.<lang>.txt, so they can
-- also just be edited by hand.
return {
	"barreiroleo/ltex_extra.nvim",
	ft = { "gitcommit", "latex", "markdown", "tex" },
	dependencies = { "neovim/nvim-lspconfig" },
	init = function()
		local dict_path = vim.fn.stdpath("config") .. "/spell/ltex"
		vim.fn.mkdir(dict_path, "p")

		local initialized = false
		vim.api.nvim_create_autocmd("LspAttach", {
			callback = function(args)
				local client = vim.lsp.get_client_by_id(args.data.client_id)
				if not client or client.name ~= "ltex" or initialized then
					return
				end
				initialized = true
				require("ltex_extra").setup({
					load_langs = { "en-US", "fr" },
					init_check = true,
					path = dict_path,
					log_level = "none",
				})
			end,
		})
	end,
}
