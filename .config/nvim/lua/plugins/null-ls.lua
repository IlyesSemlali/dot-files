-- The null-ls is a complement to Mason and allows to install automatically
-- formatters (which Mason cannot do on its own)
return {
  {
    "jay-babu/mason-null-ls.nvim",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "williamboman/mason.nvim",
      "nvimtools/none-ls.nvim",
    },
    config = function()
      local null_ls = require "mason-null-ls"
      null_ls.setup {
        automatic_installation = true,
        ensure_installed = {
          "black",
          "stylua",
          "jq",
          "terraform_fmt",
        },
      }
    end,
  },
}
