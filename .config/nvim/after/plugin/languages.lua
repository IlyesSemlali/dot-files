-- Setup lspconfig.

require'lspconfig'.bashls.setup{}

require'lspconfig'.ansiblels.setup{
    filetypes = { "yaml.ansible" }
}
