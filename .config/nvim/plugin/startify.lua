vim.g.startify_custom_header = {
'                                       .                    ',
'                       ##############..... ##############   ',
'                       ##############......##############   ',
'                         ##########..........##########     ',
'                         ##########........##########       ',
'                         ##########.......##########        ',
'                         ##########.....##########..        ',
'                         ##########....##########.....      ',
'                       ..##########..##########.........    ',
'                     ....##########.#########.............  ',
'                       ..################JJJ............    ',
'                         ################.............      ',
'                         ##############.JJJ.JJJJJJJJJJ      ',
'                         ############...JJ...JJ..JJ  JJ     ',
'                         ##########....JJ...JJ..JJ  JJ      ',
'                         ########......JJJ..JJJ JJJ JJJ     ',
'                         ######    .........                ',
'                                     .....                  ',
'                                       .                    '}

vim.g.startify_session_persistence = 1
vim.g.startify_files_number = 4

vim.g.startify_bookmarks = {
	'~/.config/nvim/init.lua',
	'~/.zshrc',
	'~/.zshrc.local',
}

vim.g.startify_lists = {
	{ type = 'bookmarks', header = { '   Bookmarks' }},
	{ type = 'sessions',  header = { '   Sessions' }},
	{ type = 'dir',       header = { '   Recent files in ' .. vim.fn.getcwd() }},
	{ type = 'files',     header = { '   Recent files' }},
	{ type = 'commands',  header = { '   Commands'}},
}
