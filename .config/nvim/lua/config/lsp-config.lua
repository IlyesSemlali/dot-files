-- The LSP Config will configure NVim's builtin LSP capabilities with the matching default
-- (and overriden) parameters (what happens on LSP Server attach, their capabilities and so on).
-- To install LSP Servers use Mason, which is configured in ~/.config/nvim/lua/plugins/mason.lua

local lspconfig = vim.lsp.config
local capabilities = require("cmp_nvim_lsp").default_capabilities()

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

lspconfig("ansiblels", {
	capabilities = capabilities,

	filetypes = { "yaml.ansible" },
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
