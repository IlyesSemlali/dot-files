-- Native NVim configurations
require("config.options")
require("config.mappings")
require("config.autocmds")

-- Plugin based configurations
require("config.lazy")
require("config.diagnostics")
require("config.lsp-config")

-- Custom uncommited config
local custom_config = vim.fn.stdpath("config") .. "/lua/config/custom.lua"
if vim.fn.filereadable(custom_config) == 1 then
	require("config.custom")
end

require("config.filetypes")
