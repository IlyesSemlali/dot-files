local on_attach = require("plugins.configs.lspconfig").on_attach
local capabilities = require("plugins.configs.lspconfig").capabilities

---@diagnostic disable-next-line: different-requires
local lspconfig = require "lspconfig"

local servers = {
  'bashls',
  'html',
  'ltex-ls',
  'lua_ls',
  'pylsp',
  'rust_analyzer',
  'terraformls',
}


for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup {
    on_attach = on_attach,
    capabilities = capabilities,
  }
end

lspconfig.pylsp.setup{
  settings = {
    pylsp = {
      plugins = {
        pycodestyle = {
          ignore = {'W391'},
          maxLineLength = 100
        }
      }
    }
  }
}

lspconfig.ltex.setup{
  settings = {
    ltex = {
      language = "en-GB",
    },
  },
}

lspconfig.bashls.setup{}

lspconfig.terraformls.setup{}

lspconfig.ansiblels.setup{
  filetypes = { "yaml.ansible" }
}

lspconfig.pyright.setup{
  settings = {
    pyright = {
        typeCheckingMode = "off",
      }
  }
}

lspconfig.yamlls.setup {
  settings = {
    yaml = {
      schemas = {
        ["https://raw.githubusercontent.com/instrumenta/kubernetes-json-schema/master/v1.18.0-standalone-strict/all.json"] = "/*.yaml",
      },
    },
  }
}
