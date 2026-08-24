return {
	{
		"olimorris/codecompanion.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-treesitter/nvim-treesitter",
		},
		opts = {
			interactions = {
				-- Inline uses ACP: auth via `claude login`
				chat = {
					adapter = "claude_code",
				},
				-- Inline uses ACP: auth via `claude login`
				inline = {
					adapter = "claude_code",
				},
			},
			display = {
				diff = {
					provider = "default", -- native vimdiff split layout
				},
			},
			adapters = {
				-- ACP adapter — tunnels through Claude Code CLI (claude login)
				--    Supports: chat, multi-file edits
				--    Does NOT support: inline buffer diffs
				acp = {
					claude_code = function()
						return require("codecompanion.adapters").extend("claude_code", {
							env = {
								-- Populated automatically by `claude login`
								CLAUDE_CODE_OAUTH_TOKEN = "CLAUDE_CODE_OAUTH_TOKEN",
							},
						})
					end,
				},
			},
		},
		keys = {
			-- Toggle AI chat panel (uses ACP / claude login)
			{
				"<leader>cc",
				"<cmd>CodeCompanionChat Toggle<cr>",
				desc = "CodeCompanion: Toggle Chat",
				mode = { "n", "v" },
			},
			-- Inline prompt: streams diff directly into the buffer (uses HTTP / API key)
			{ "<leader>ci", "<cmd>CodeCompanion<cr>", desc = "CodeCompanion: Inline Prompt", mode = { "n", "v" } },
		},
	},
}
