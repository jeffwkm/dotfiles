require("config.globals")

return {
  {
    "hrsh7th/vim-vsnip",
    dependencies = {
      "hrsh7th/vim-vsnip-integ",
    },
    config = function()
      if not vim.g.vscode then
      end
    end,
  },
  {
    "hrsh7th/cmp-vsnip",
    config = function()
      if not vim.g.vscode then
      end
    end
  },
  {
    "hrsh7th/cmp-nvim-lsp",
    config = function()
      if not vim.g.vscode then
      end
    end
  },
  {
    "hrsh7th/nvim-cmp",
    config = function()
      if not vim.g.vscode then
        local cmp = require("cmp")
        cmp.setup({
          snippet = {
            -- REQUIRED - you must specify a snippet engine
            expand = function(args)
              vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
              -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
              -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
              -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
              -- vim.snippet.expand(args.body) -- For native neovim snippets (Neovim v0.10+)
            end,
          },
          completion = cmp.config.window.bordered(),
          window = {
            documentation = cmp.config.window.bordered(),
          },
          mapping = cmp.mapping.preset.insert({
            ['<C-p>'] = cmp.mapping.scroll_docs(-4),
            ['<C-n>'] = cmp.mapping.scroll_docs(4),
            ['<Tab>'] = cmp.mapping.complete(),
            ['<C-g>'] = cmp.mapping.abort(),
            ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
          }),
          sources = cmp.config.sources({
            { name = 'nvim_lsp' },
            { name = 'vsnip' }, -- For vsnip users.
            -- { name = 'luasnip' }, -- For luasnip users.
            -- { name = 'ultisnips' }, -- For ultisnips users.
            -- { name = 'snippy' }, -- For snippy users.
          }, {
            { name = 'buffer' },
          })
        })
      end
    end
  },
  {
    "williamboman/mason.nvim",
    config = function()
      if not vim.g.vscode then
        require("mason").setup()
      end
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    config = function()
      if not vim.g.vscode then
        require("mason-lspconfig").setup({
          ensure_installed = { "lua_ls", "rust_analyzer" },
          -- automatic_installation = true,
        })
      end
    end,
  },
  {
    "neovim/nvim-lspconfig",
    config = function()
      if not vim.g.vscode then
        local lspconfig = require('lspconfig')

        lspconfig.lua_ls.setup {
          on_init = function(client)
            local path = client.workspace_folders[1].name
            if vim.loop.fs_stat(path .. '/.luarc.json') or vim.loop.fs_stat(path .. '/.luarc.jsonc') then
              return
            end

            client.config.settings.Lua = vim.tbl_deep_extend('force', client.config.settings.Lua, {
              runtime = {
                -- Tell the language server which version of Lua you're using
                -- (most likely LuaJIT in the case of Neovim)
                version = 'LuaJIT'
              },
              -- Make the server aware of Neovim runtime files
              workspace = {
                checkThirdParty = false,
                library = {
                  vim.env.VIMRUNTIME
                  -- Depending on the usage, you might want to add additional paths here.
                  -- "${3rd}/luv/library"
                  -- "${3rd}/busted/library",
                }
                -- or pull in all of 'runtimepath'. NOTE: this is a lot slower
                -- library = vim.api.nvim_get_runtime_file("", true)
              }
            })
          end,
          settings = {
            Lua = {}
          }
        }

        lspconfig.rust_analyzer.setup({})
      end
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter",
    config = function()
      require('nvim-treesitter').setup();
      require('nvim-treesitter.configs').setup {
        ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "markdown", "markdown_inline", "clojure", "bash", "css", "python", "nix", "json", "toml", "rust", "diff", "gitcommit", "gitignore", "nginx", "properties", "regex", "ruby", "sql", "ssh_config", "tmux", "toml", "tsx", "typescript", "xml", "yaml" },
        auto_install = true,
        highlight = {
          enable = true,
          additional_vim_regex_highlighting = false,
        },
        indent = {
          enable = true
        },
        incremental_selection = {
          enable = false,
          keymaps = {
            init_selection = "gnn", -- set to `false` to disable one of the mappings
            node_incremental = "grn",
            ccope_incremental = "grc",
            node_decremental = "grm",
          },
        },
      }
      vim.cmd('TSUpdate')
    end,
  },
  {
    "julienvincent/nvim-paredit",
    config = function()
      require('nvim-treesitter').setup()
      require("nvim-paredit").setup()
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
