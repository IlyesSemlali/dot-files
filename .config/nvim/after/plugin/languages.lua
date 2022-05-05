-- Setup lspconfig.

require'lspconfig'.bashls.setup{}

require'lspconfig'.terraformls.setup{}

require'lspconfig'.ansiblels.setup{
    filetypes = { "yaml.ansible" }
}

require('lspconfig').yamlls.setup {
  settings = {
    yaml = {
      schemas = {
        ["https://raw.githubusercontent.com/instrumenta/kubernetes-json-schema/master/v1.23.0-standalone-strict/all.json"] = "/*.yaml",
      },
    },
  }
}
