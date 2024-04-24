return {
  defaults = {
    layout_config = {},
    preview = {
      -- To show previews of pictures
      mime_hook = function(filepath, bufnr, opts)
        local is_image = function(filepath)
          local image_extensions = { "png", "jpg" } -- Supported image formats
          local split_path = vim.split(filepath:lower(), ".", { plain = true })
          local extension = split_path[#split_path]
          return vim.tbl_contains(image_extensions, extension)
        end
        if is_image(filepath) then
          local term = vim.api.nvim_open_term(bufnr, {})
          local function send_output(_, data, _)
            for _, d in ipairs(data) do
              vim.api.nvim_chan_send(term, d .. "\r\n")
            end
          end
          vim.fn.jobstart({
            "catimg",
            filepath, -- Terminal image viewer command
          }, { on_stdout = send_output, stdout_buffered = true, pty = true })
        else
          require("telescope.previewers.utils").set_preview_message(bufnr, opts.winid, "Binary cannot be previewed")
        end
      end,
    },
  },

  pickers = {
    diagnostics = {
      layout_strategy = "vertical",
      layout_config = {
        mirror = true,
        prompt_position = "top",
        preview_cutoff = 0,
      },

      previewer = require("telescope.previewers").new_buffer_previewer {
        title = "Diagnostics",
        dyn_title = function(_, entry)
          return entry.title
        end,

        get_buffer_by_name = function(_, entry)
          return "diagnostics_" .. tostring(entry.nr)
        end,

        define_preview = function(self, entry)
          vim.api.nvim_buf_set_lines(self.state.bufnr, 0, -1, false, { entry.text })
        end,
      },
    },
  },
  extensions = {},
}
