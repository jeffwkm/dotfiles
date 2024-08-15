require("config.globals")

return {
  {
    "julienvincent/nvim-paredit",
    config = function()
    end,
  },
  {
    "easymotion/vim-easymotion",
    init = function()
      vim.g.EasyMotion_do_mapping = 0
    end,
    config = function()
      vim.g.EasyMotion_startofline = 0
      vim.g.EasyMotion_verbose = 0
      vim.g.EasyMotion_show_prompt = 0
      vim.g.EasyMotion_prompt = ''
      Knov('s', '<Plug>(easymotion-s2)', { silent = true })
      Knov('S', '<Plug>(easymotion-s2)', { silent = true })
      Knov('f', '<Plug>(easymotion-f)')
      Knov('F', '<Plug>(easymotion-F)')
      Knov('t', '<Plug>(easymotion-tl)')
      Knov('T', '<Plug>(easymotion-Tl)')
      Knov('gsj', '<Plug>(easymotion-j)')
      Knov('gsk', '<Plug>(easymotion-k)')
    end,
  },
  {
    "tpope/vim-surround",
    config = function()
    end,
  },
  {
    "tpope/vim-repeat",
    config = function()
    end,
  },
  {
    "tpope/vim-commentary",
    cond = not vim.g.vscode,
    config = function()
    end,
  },
  {
    "guns/vim-sexp",
    ft = { "clojure", "scheme", "lisp" },
    config = function()
    end,
  },
}
