-- MacOS specific clipboard settings
--
-- these are set explicitely since when using
-- nix darwin for NeoVim prevents it to have access
-- to the pbcopy/pbpaste without the full path
--
if vim.loop.os_uname().sysname == "Darwin" then
	vim.g.clipboard = {
		name = "macOS-clipboard",
		copy = {
			["+"] = { "/usr/bin/pbcopy" },
			["*"] = { "/usr/bin/pbcopy" },
		},
		paste = {
			["+"] = { "/usr/bin/pbpaste" },
			["*"] = { "/usr/bin/pbpaste" },
		},
		cache_enabled = 0,
	}
end

vim.opt.clipboard = ""
