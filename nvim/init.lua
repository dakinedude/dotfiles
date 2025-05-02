vim.o.clipboard = 'unnamedplus'

vim.o.number = true       -- Show line numbers
vim.o.relativenumber = true  -- Relative line numbers
vim.o.tabstop = 4         -- Number of spaces a tab counts for
vim.o.shiftwidth = 4      -- Indentation amount for autoindent
vim.o.expandtab = true    -- Convert tabs to spaces
vim.o.smartindent = true  -- Autoindent new lines

vim.cmd 'syntax on'

vim.cmd 'filetype plugin indent on'

vim.api.nvim_set_keymap('n', '<C-s>', ':w<CR>', { noremap = true })
vim.api.nvim_set_keymap('i', 'jj', '<Esc>', { noremap = true })

