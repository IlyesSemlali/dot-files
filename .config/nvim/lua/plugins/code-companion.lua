return {
	{
		"olimorris/codecompanion.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-treesitter/nvim-treesitter",
		},
		opts = {
			interactions = {
				-- Chat uses ACP: no per-token billing, auth via `claude login`
				chat = {
					adapter = "claude_code",
				},
				-- Inline uses HTTP: ACP doesn't support streaming buffer diffs
				-- Requires $ANTHROPIC_API_KEY (personal or enterprise workspace key)
				inline = {
					adapter = "anthropic",
				},
			},
			display = {
				diff = {
					provider = "default", -- native vimdiff split layout
				},
			},
			adapters = {
				-- 1. ACP adapter — tunnels through Claude Code CLI (claude login)
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
				-- 2. HTTP adapter — standard Anthropic API
				--    Used only for inline (<leader>ci), since ACP can't do inline
				--    Set $ANTHROPIC_API_KEY to your personal or enterprise API key
				http = {
					anthropic = function()
						return require("codecompanion.adapters").extend("anthropic", {
							env = {
								api_key = "ANTHROPIC_API_KEY",
							},
							schema = {
								model = {
									default = "claude-sonnet-5",
								},
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
			-- Open action palette (explain, refactor, generate tests, …)
			{
				"<leader>ca",
				"<cmd>CodeCompanionActions<cr>",
				desc = "CodeCompanion: Action Palette",
				mode = { "n", "v" },
			},
			-- Inline prompt: streams diff directly into the buffer (uses HTTP / API key)
			{ "<leader>ci", "<cmd>CodeCompanion<cr>", desc = "CodeCompanion: Inline Prompt", mode = { "n", "v" } },
		},
	},
}
