return {
	{
		"hrsh7th/nvim-cmp",
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
		},

		config = function()
			-- this allows to use friendly-snippets with luasnip
			require("luasnip.loaders.from_vscode").lazy_load()

			local luasnip = require("luasnip")
			local cmp = require("cmp")
			local map = vim.keymap.set

			map({ "i", "s" }, "<C-H>", function()
				require("luasnip").jump(-1)
			end, { silent = true })

			map({ "i", "s" }, "<C-L>", function()
				require("luasnip").jump(1)
			end, { silent = true })

			cmp.setup({
				snippet = {
					expand = function(args)
						luasnip.lsp_expand(args.body)
					end,
				},

				sources = {
					{ name = "nvim_lsp", keyword_length = 1 },
					{ name = "path", max_item_count = 4 },
					{ name = "buffer", keyword_length = 4, max_item_count = 4 },
					{ name = "luasnip", keyword_length = 2 },
				},

				window = {
					completion = cmp.config.window.bordered({
						border = "rounded",
					}),
					documentation = cmp.config.window.bordered({
						border = "rounded",
					}),
				},

				formatting = {
					fields = { "menu", "abbr", "kind" },
					format = function(entry, item)
						local menu_icon = {
							nvim_lsp = "λ",
							luasnip = "⋗",
							buffer = "Ω",
							path = "~",
						}

						item.menu = menu_icon[entry.source.name]
						return item
					end,
				},

				completion = {
					keyword_length = 2,
					completeopt = "menu,menuone,preview,noselect,noinsert",
				},

				matching = {
					disallow_fuzzy_matching = false,
					disallow_partial_matching = false,
					disallow_prefix_unmatching = false,
				},

				mapping = {
					--
					["<CR>"] = cmp.config.disable,

					["<Tab>"] = cmp.mapping(function(fallback)
						if cmp.visible() then
							cmp.select_next_item()
						else
							fallback()
						end
					end, { "i", "s" }),

					["<S-Tab>"] = cmp.mapping(function(fallback)
						if cmp.visible() then
							cmp.select_prev_item()
						else
							fallback()
						end
					end, { "i", "s" }),
					--
					["<Up>"] = cmp.mapping.select_prev_item(),
					["<Down>"] = cmp.mapping.select_next_item(),

					["<C-Space>"] = cmp.mapping.confirm({ select = true }),

					["<C-u>"] = cmp.mapping.scroll_docs(-4),
					["<C-d>"] = cmp.mapping.scroll_docs(4),

					["<C-e>"] = cmp.mapping.abort(),
				},
			})
		end,
	},
	{
		"L3MON4D3/LuaSnip",
		dependencies = { "rafamadriz/friendly-snippets", "saadparwaiz1/cmp_luasnip" },
	},
}
