---@type ChadrcConfig
local M = {}

-- Plugins
M.plugins = "custom.plugins"

-- Mappings
M.mappings = require "custom.mappings"

-- UI
M.ui = {
  theme = "gruvbox";
  transparency = true
}

return M
