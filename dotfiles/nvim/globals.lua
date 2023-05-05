local fn = vim.fn
local api = vim.api

-- Inspect something
function _G.inspect(item)
  vim.print(item)
end

vim.g.logging_level = "info"

-- set leader key to space
vim.g.mapleader = " "
