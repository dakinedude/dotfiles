vim.o.clipboard = 'unnamedplus'

-- Set the font for Neovim GUI (if using a GUI like Neovide)
-- vim.o.guifont = "Source Code Pro Nerd Font Medium:h14"

-- Basic settings
vim.o.number = true       -- Show line numbers
vim.o.relativenumber = true  -- Relative line numbers
vim.o.tabstop = 4         -- Number of spaces a tab counts for
vim.o.shiftwidth = 4      -- Indentation amount for autoindent
vim.o.expandtab = true    -- Convert tabs to spaces
vim.o.smartindent = true  -- Autoindent new lines

-- Syntax highlighting
vim.cmd 'syntax on'

-- Enable filetype detection and plugins
vim.cmd 'filetype plugin indent on'

-- Basic key mappings
vim.api.nvim_set_keymap('n', '<C-s>', ':w<CR>', { noremap = true })
vim.api.nvim_set_keymap('i', 'jj', '<Esc>', { noremap = true })

