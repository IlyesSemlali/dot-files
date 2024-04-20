-- require('luasnip.loaders.from_vscode').lazy_load()

local cmp = require "cmp"
local luasnip = require "luasnip"

local select_opts = { behavior = cmp.SelectBehavior.Select }

return {
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

  window = {
    completion = cmp.config.window.bordered(),
    documentation = cmp.config.window.bordered(),
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

    ["<C-Space>"] = cmp.mapping.confirm { select = true },

    ["<C-u>"] = cmp.mapping.scroll_docs(-4),
    ["<C-d>"] = cmp.mapping.scroll_docs(4),

    ["<C-e>"] = cmp.mapping.abort(),
  },
}
