-- The LSP Config will configure NVim's builtin LSP capabilities with the matching default
-- (and overriden) parameters (what happens on LSP Server attach, their capabilities and so on).
-- To install LSP Servers use Mason, which is configured in ~/.config/nvim/lua/plugins/mason.lua

local lspconfig = vim.lsp.config
local capabilities = require("cmp_nvim_lsp").default_capabilities()

vim.lsp.set_log_level("off") -- Turns off Logs on LSP
-- vim.lsp.set_log_level("debug") -- Turns on Logs on LSP

capabilities.textDocument.completion.completionItem = {
	documentationFormat = { "markdown", "plaintext" },
	snippetSupport = true,
	preselectSupport = true,
	insertReplaceSupport = true,
	labelDetailsSupport = true,
	deprecatedSupport = true,
	commitCharactersSupport = true,
	tagSupport = { valueSet = { 1 } },
	resolveSupport = {
		properties = {
			"documentation",
			"detail",
			"additionalTextEdits",
		},
	},
}

-- Disable LSP progress globally
vim.lsp.handlers["$/progress"] = function() end

-- To install those servers, also add them in the ensure_installed section of
-- mason-lspconfig plugin
local servers = {
	"bashls",
	"html",
	"jinja_lsp",
	"ltex",
	"lua_ls",
	"rust_analyzer",
	"terraformls",
}

-- lsps with default config
for _, lsp in ipairs(servers) do
	lspconfig(lsp, {
		capabilities = capabilities,
	})
end

lspconfig("ltex", {
	filetypes = {
		"gitcommit",
		"latex",
		"markdown",
		"tex",
	},
	capabilities = capabilities,
	-- If you need to disable a specific rule, set the log level to debug for the LSP:
	-- on_attach = function()
	-- 	vim.lsp.set_log_level("debug")
	-- end,

	settings = {
		ltex = {
			language = "auto",
			disabledRules = {
				["fr"] = { "FRENCH_WHITESPACE" },
			},
			--
		},
	},
})

lspconfig("terraformls", {
	capabilities = capabilities,

	filetypes = { "terraform", "tf.jinja" },
})

lspconfig("jinja_lsp", {
	capabilities = capabilities,

	filetypes = { "tf.jinja" },
})

lspconfig("ansiblels", {
	capabilities = capabilities,

	filetypes = { "yaml.ansible" },
})

lspconfig("lua_ls", {
	settings = {
		Lua = {
			runtime = {
				-- Tell the language server which version of Lua you're using
				-- (most likely LuaJIT in the case of Neovim)
				version = "LuaJIT",
			},
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = {
					"vim",
					"require",
				},
			},
			workspace = {
				-- Make the server aware of Neovim runtime files
				library = vim.api.nvim_get_runtime_file("", true),
			},
			-- Do not send telemetry data containing a randomized but unique identifier
			telemetry = {
				enable = false,
			},
		},
	},
})

lspconfig("ruff", {
	capabilities = capabilities,
})

lspconfig("pylsp", {
	capabilities = capabilities,

	settings = {
		pylsp = {
			plugins = {
				pycodestyle = { enabled = false }, -- handled by ruff
				pylint = { enabled = false }, -- handled by ruff
				black = {
					enabled = true,
					line_length = 100,
				},
				ruff = {
					enabled = true,
				},
				jedi_completion = {
					fuzzy = true,
				},
			},
		},
	},
})

lspconfig("yamlls", {
	capabilities = capabilities,

	settings = {
		yaml = {
			validate = true,
			-- disable the schema store
			schemaStore = {
				enable = false,
				url = "",
			},
			-- manually select schemas
			schemas = {
				["https://raw.githubusercontent.com/docker/compose/master/compose/config/compose_spec.json"] = "docker-compose*.{yml,yaml}",
				["https://json.schemastore.org/github-workflow.json"] = ".github/workflows/*.{yml,yaml}",
			},
		},
	},
})

lspconfig("helm_ls", {
	capabilities = capabilities,

	settings = {
		["helm-ls"] = {
			yamlls = {
				path = "yaml-language-server",
			},
		},
	},
})
