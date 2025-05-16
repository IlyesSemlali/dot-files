return {
	{
		"machakann/vim-sandwich",
		lazy = false,
		config = function()
			local global = vim.g

			-- Load default recipes
			vim.cmd("runtime macros/sandwich/recipes.vim")
			local recipes = global["sandwich#default_recipes"] or {}

			-- Add your custom recipe
			table.insert(recipes, {
				buns = { "*", "*" },
				kind = { "add", "replace" },
				action = { "add", "replace" },
				input = { "*" },
				linewise = 0,
			})

			-- Assign final recipe list
			global["sandwich#recipes"] = recipes
		end,
	},
}
