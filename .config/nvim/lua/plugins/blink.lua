return {
	{
		"saghen/blink.cmp",
		dependencies = {
			"L3MON4D3/LuaSnip",
			"rafamadriz/friendly-snippets",
		},
		version = "*",

		---@module 'blink.cmp'
		---@type blink.cmp.Config
		opts = {
			keymap = {
				preset = "none",
				["<C-space>"] = { "show", "accept", "fallback" },
				["<C-e>"] = { "hide", "fallback" },
				["<Tab>"] = { "select_next", "fallback" },
				["<S-Tab>"] = { "select_prev", "fallback" },
				["<Up>"] = { "select_prev", "fallback" },
				["<Down>"] = { "select_next", "fallback" },
				["<C-u>"] = { "scroll_documentation_up", "fallback" },
				["<C-d>"] = { "scroll_documentation_down", "fallback" },
				["<C-l>"] = { "snippet_forward", "fallback" },
				["<C-h>"] = { "snippet_backward", "fallback" },
			},

			snippets = {
				preset = "luasnip",
			},

			appearance = {
				use_nvim_cmp_as_default = true,
				nerd_font_variant = "mono",
			},

			completion = {
				menu = {
					border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
					winhighlight = "Normal:NormalFloat,FloatBorder:FloatBorder,CursorLine:PmenuSel,Search:None",

					draw = {
						columns = {
							{ "label" },
							{ "label_description", "kind_icon", "kind", gap = 1 },
						},
						components = {
							label_description = {
								text = function(ctx)
									local source_icons = {
										lsp = "λ",
										snippets = "⋗",
										buffer = "Ω",
										path = "~",
									}
									return (source_icons[ctx.source_name] or ctx.source_name) .. " "
								end,
								highlight = "BlinkCmpLabelDescription",
							},
							kind_icon = {
								ellipsis = false,
								text = function(ctx)
									return ctx.kind_icon .. " "
								end,
								highlight = function(ctx)
									return ctx.kind_hl
								end,
							},
						},
					},
				},
			},

			sources = {
				default = { "lsp", "path", "snippets", "buffer" },
				providers = {
					lsp = { min_keyword_length = 1 },
					path = { max_items = 4 },
					buffer = { min_keyword_length = 3, max_items = 4 },
					snippets = { min_keyword_length = 2 },
				},
			},
		},
		config = function(_, opts)
			require("luasnip.loaders.from_vscode").lazy_load()
			require("blink.cmp").setup(opts)
		end,
	},

	{
		"L3MON4D3/LuaSnip",
		version = "v2.*",
	},
}
