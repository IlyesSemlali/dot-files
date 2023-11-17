---@diagnostic disable: inject-field
local M = {}

-- Plugins
M.plugins = "custom.plugins"

-- Mappings
M.mappings = require "custom.mappings"

-- UI
M.ui = {

  hl_override = {
    NvDashAscii = {
      fg = "blue",
      bg = "black",
    },

    NvDashButtons = {
      fg = "blue",
      bg = "one_bg",
    },
  },

  theme = "gruvbox";
  transparency = true,

  nvdash = {
    load_on_startup = true,

    header = {
      "          ________ ++     ________          ",
      "         /VVVVVVVV\\++++  /VVVVVVVV\\         ",
      "         \\VVVVVVVV/++++++\\VVVVVVVV/         ",
      "          |VVVVVV|++++++++/VVVVV/'          ",
      "          |VVVVVV|++++++/VVVVV/'            ",
      "         +|VVVVVV|++++/VVVVV/'+             ",
      "       +++|VVVVVV|++/VVVVV/'+++++           ",
      "     +++++|VVVVVV|/VVVVV/'+++++++++         ",
      "       +++|VVVVVVVVVVV/'+++++++++           ",
      "         +|VVVVVVVVV/'+++++++++             ",
      "          |VVVVVVV/'+++++++++               ",
      "          |VVVVV/'+++++++++                 ",
      "          |VVV/'+++++++++                   ",
      "          'V/'   ++++++                     ",
      "                   ++                       ",
    },

    buttons = {
      { "  Find File", "Spc f f", "Telescope find_files" },
      { "󰈚  Recent Files", "Spc f o", "Telescope oldfiles" },
      { "󰈭  Find Word", "Spc f w", "Telescope live_grep" },
      { "  Bookmarks", "Spc m a", "Telescope marks" },
      { "  Themes", "Spc t h", "Telescope themes" },
      { "  Mappings", "Spc c h", "NvCheatsheet" },
    },
  },


}

return M
