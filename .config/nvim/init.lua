vim.o.clipboard = 'unnamedplus'

vim.o.number = true   
vim.o.relativenumber = true  
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.expandtab = true
vim.o.smartindent = true

vim.cmd 'syntax on'

vim.cmd 'filetype plugin indent on'

vim.api.nvim_set_keymap('n', '<C-s>', ':w<CR>', { noremap = true })
vim.api.nvim_set_keymap('i', 'jj', '<Esc>', { noremap = true })

