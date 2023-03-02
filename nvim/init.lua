-- show line numbers
vim.cmd [[set number]]

-- enabling theme
vim.cmd [[colorscheme onedark]]

-- coc setup
local keyset = vim.keymap.set
-- Autocomplete
function _G.check_back_space()
    local col = vim.fn.col('.') - 1
    return col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') ~= nil
end

local opts = {silent = true, noremap = true, expr = true, replace_keycodes = false}
keyset("i", "<TAB>", 'coc#pum#visible() ? coc#pum#next(1) : v:lua.check_back_space() ? "<TAB>" : coc#refresh()', opts)
keyset("i", "<S-TAB>", [[coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"]], opts)

-- packer setup
require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'
  
  -- onedark theme
  use 'navarasu/onedark.nvim'

  -- tree-sitter
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

  --coc.nvim
  use {'neoclide/coc.nvim', branch = 'release'}
end)
