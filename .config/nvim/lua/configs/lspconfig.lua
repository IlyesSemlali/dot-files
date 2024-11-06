local on_attach = require("nvchad.configs.lspconfig").on_attach
local on_init = require("nvchad.configs.lspconfig").on_init
local capabilities = require("nvchad.configs.lspconfig").capabilities

---@diagnostic disable-next-line: different-requires
local lspconfig = require "lspconfig"

local servers = {
  "bashls",
  "html",
  "ltex",
  "lua_ls",
  "pylsp",
  "rust_analyzer",
  "terraformls",
}

-- lsps with default config
for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup {
    on_attach = on_attach,
    on_init = on_init,
    capabilities = capabilities,
  }
end

lspconfig.pylsp.setup {
  settings = {
    pylsp = {
      plugins = {
        pycodestyle = {
          ignore = { "W391" },
          maxLineLength = 100,
        },
      },
    },
  },
}

lspconfig.ltex.setup {
  settings = {
    ltex = {
      language = "en-US",
    },
  },
}

lspconfig.bashls.setup {}

lspconfig.terraformls.setup {}

lspconfig.ansiblels.setup {
  filetypes = { "yaml.ansible" },
}

lspconfig.pyright.setup {
  settings = {
    pyright = {
      typeCheckingMode = "off",
    },
  },
}

lspconfig.yamlls.setup {
  settings = {
    yaml = {
      validate = true,
      -- disable the schema store
      schemaStore = {
        enable = false,
        url = "",
      },
      -- manually select schemas
      schemas = {
        ["https://raw.githubusercontent.com/docker/compose/master/compose/config/compose_spec.json"] = "docker-compose*.{yml,yaml}",
        ["https://json.schemastore.org/github-workflow.json"] = ".github/workflows/*.{yml,yaml}",
      },
    },
  },
}

lspconfig.helm_ls.setup {
  settings = {
    ["helm-ls"] = {
      yamlls = {
        path = "yaml-language-server",
      },
    },
  },
}
