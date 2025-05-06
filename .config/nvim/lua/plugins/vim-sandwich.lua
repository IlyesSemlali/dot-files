return {
	{
		"machakann/vim-sandwich",
		lazy = false,
		config = function()
			local global = vim.g
			local recipes = global["sandwich#default_recipes"]

			table.insert(recipes, {
				buns = { "*", "*" },
				kind = { "add", "replace" },
				action = { "add", "replace" },
				input = { "*" },
				linewise = 0,
			})

			global["sandwich#recipes"] = recipes
		end,
	},
}
