-- Jinja
vim.filetype.add({
	pattern = {
		[".*%.tf%.j2"] = "tf.jinja",
	},
})

vim.treesitter.language.register("terraform", "tf.jinja")
