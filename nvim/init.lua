-- nvim setup
vim.cmd [[set number]]
vim.cmd [[set shiftwidth=4]]
vim.cmd [[colorscheme onedark]]

-- coc setup
local keyset = vim.keymap.set
local opts = {silent = true, noremap = true, expr = true, replace_keycodes = false}
keyset("i", "<TAB>", [[coc#pum#visible() ? coc#pum#confirm() : coc#on_enter()]], opts)

-- packer setup
require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'
  
  -- onedark theme
  use 'navarasu/onedark.nvim'

  -- fish syntax highlighting
  use 'dag/vim-fish'

  -- ron syntax highlighting
  use 'ron-rs/ron.vim'

  -- tree-sitter
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

  --coc.nvim
  use {'neoclide/coc.nvim', branch = 'release'}

  --markdown preview
  use({
    "iamcco/markdown-preview.nvim",
    run = function() vim.fn["mkdp#util#install"]() end,
  })
end)
