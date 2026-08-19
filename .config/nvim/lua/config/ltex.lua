-- Restricts ltex-ls's language auto-detection to just French and English.
--
-- ltex-ls's own `language = "auto"` detects among every language LanguageTool
-- knows, and per its docs only ever resolves to a generic code (e.g. "en"
-- instead of "en-US"), which means spelling errors stop being reported. So
-- instead we do our own cheap two-way detection and push the result to the
-- running client via `workspace/didChangeConfiguration`.

local M = {}

local function split_words(s)
	local t = {}
	for w in s:gmatch("%S+") do
		t[w] = true
	end
	return t
end

local FRENCH_WORDS = split_words(
	"le la les un une des du de et est être avoir dans pour avec sur pas que qui mais où ne se ça "
		.. "cette ce ces nous vous ils elle elles leur tout comme aussi très donc fait sont était"
)
local ENGLISH_WORDS = split_words(
	"the and is are was were have has with for not that which but where this these those we you "
		.. "they their all more also very well then do does did will would can could should of to in it a an"
)

local FRENCH_ACCENTS = "[éèêëàâîïôùûüçœÉÈÊËÀÂÎÏÔÙÛÜÇŒ]"
local MIN_SAMPLE_LEN = 20
local SAMPLE_LIMIT = 4000
local DEBOUNCE_MS = 800

-- Both the prose client (markdown/latex/tex) and the gitcommit-only one
-- (see lsp-config.lua) get live French/English switching.
local CLIENT_NAMES = { ltex = true, ltex_gitcommit = true }

-- Returns "fr", "en-US", or nil (not enough signal, keep the current language).
function M.detect(bufnr)
	local text = table.concat(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false), " "):sub(1, SAMPLE_LIMIT)
	if #text < MIN_SAMPLE_LEN then
		return nil
	end

	local _, accents = text:gsub(FRENCH_ACCENTS, "")
	if accents / #text > 0.01 then
		return "fr"
	end

	local fr_score, en_score = 0, 0
	for word in text:lower():gmatch("%a[%a'-]*") do
		if FRENCH_WORDS[word] then
			fr_score = fr_score + 1
		elseif ENGLISH_WORDS[word] then
			en_score = en_score + 1
		end
	end
	if fr_score == en_score then
		return nil
	end
	return fr_score > en_score and "fr" or "en-US"
end

local function ltex_clients(bufnr)
	local clients = {}
	for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
		if CLIENT_NAMES[client.name] then
			table.insert(clients, client)
		end
	end
	return clients
end

local function check_buffer(bufnr)
	if not vim.api.nvim_buf_is_valid(bufnr) then
		return
	end
	local clients = ltex_clients(bufnr)
	local lang = #clients > 0 and M.detect(bufnr)
	if not lang then
		return
	end
	for _, client in ipairs(clients) do
		local settings = client.config.settings
		if settings and settings.ltex and settings.ltex.language ~= lang then
			settings.ltex.language = lang
			client:notify("workspace/didChangeConfiguration", { settings = settings })
		end
	end
end

local timers = {}
local function debounced_check(bufnr)
	if timers[bufnr] then
		timers[bufnr]:stop()
	end
	timers[bufnr] = vim.defer_fn(function()
		timers[bufnr] = nil
		check_buffer(bufnr)
	end, DEBOUNCE_MS)
end

function M.setup()
	vim.api.nvim_create_autocmd("LspAttach", {
		callback = function(args)
			local client = vim.lsp.get_client_by_id(args.data.client_id)
			if not client or not CLIENT_NAMES[client.name] then
				return
			end
			check_buffer(args.buf)
			vim.api.nvim_create_autocmd({ "TextChanged", "InsertLeave" }, {
				buffer = args.buf,
				callback = function()
					debounced_check(args.buf)
				end,
			})
		end,
	})
end

return M
