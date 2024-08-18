require("config.globals")

return {
  {
    'projekt0n/github-nvim-theme',
    lazy = false,
    priority = 1000,
    enabled = not vim.g.vscode,
    config = function()
      require('github-theme').setup({
        options = {
          transparent = true,
        }
      })
      vim.cmd('colorscheme github_dark')
    end,
  },
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.8',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'BurntSushi/ripgrep',
      'sharkdp/fd',
      'nvim-treesitter/nvim-treesitter',
      'nvim-tree/nvim-web-devicons'
    }
  },
  {
    "nvim-neorg/neorg",
    lazy = false,
    version = "*",
    config = function()
      require("neorg").setup {
        load = {
          ["core.defaults"] = {},
          ["core.concealer"] = {},
          ["core.dirman"] = {
            config = {
              workspaces = {
                notes = "~/notes",
              },
              default_workspace = "notes",
            },
          },
        },
      }

      vim.wo.foldlevel = 99
      vim.wo.conceallevel = 2
    end,
  },
  {
    "NeogitOrg/neogit",
    enabled = not vim.g.vscode,
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
      "nvim-telescope/telescope.nvim",
    },
    config = function()
      require("neogit").setup({
        disable_hint = true,
        -- graph_style = "ascii",
        graph_style = "unicode"
      })
    end
  },
  {
    "rebelot/kanagawa.nvim",
    enabled = not vim.g.vscode,
    config = function()
      require("kanagawa").setup({})
      -- vim.cmd.colorscheme "kanagawa"
    end
  },
  {
    "folke/noice.nvim",
    enabled = not vim.g.vscode,
    event = "VeryLazy",
    opts = {
      -- add any options here
    },
    dependencies = {
      -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
      "MunifTanjim/nui.nvim",
      -- OPTIONAL:
      --   `nvim-notify` is only needed, if you want to use the notification view.
      --   If not available, we use `mini` as the fallback
      "rcarriga/nvim-notify",
    },
    config = function()
      require("noice").setup({
        lsp = {
          -- override markdown rendering so that **cmp** and other plugins use **Treesitter**
          override = {
            ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
            ["vim.lsp.util.stylize_markdown"] = true,
            ["cmp.entry.get_documentation"] = true, -- requires hrsh7th/nvim-cmp
          },
        },
        -- you can enable a preset for easier configuration
        presets = {
          bottom_search = true,         -- use a classic bottom cmdline for search
          command_palette = true,       -- position the cmdline and popupmenu together
          long_message_to_split = true, -- long messages will be sent to a split
          inc_rename = false,           -- enables an input dialog for inc-rename.nvim
          lsp_doc_border = true,        -- add a border to hover docs and signature help
        },
      })
    end
  },
  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    enabled = not vim.g.vscode,
    config = function()
      require("catppuccin").setup({
        flavour = "mocha",
        transparent_background = true,
        custom_highlights = function(colors)
          return {
            Comment = { fg = "#ACA8B8" },
          }
        end
      })
      -- vim.cmd.colorscheme "catppuccin"
    end
  },
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    enabled = not vim.g.vscode,
    opts = {}, -- for default options, refer to the configuration section for custom setup.
    cmd = "Trouble",
    keys = {
      {
        "<leader>xx",
        "<cmd>Trouble diagnostics toggle<cr>",
        desc = "Diagnostics (Trouble)",
      },
      {
        "<leader>xX",
        "<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
        desc = "Buffer Diagnostics (Trouble)",
      },
      {
        "<leader>cs",
        "<cmd>Trouble symbols toggle focus=false<cr>",
        desc = "Symbols (Trouble)",
      },
      {
        "<leader>cl",
        "<cmd>Trouble lsp toggle focus=false win.position=right<cr>",
        desc = "LSP Definitions / references / ... (Trouble)",
      },
      {
        "<leader>xL",
        "<cmd>Trouble loclist toggle<cr>",
        desc = "Location List (Trouble)",
      },
      {
        "<leader>xQ",
        "<cmd>Trouble qflist toggle<cr>",
        desc = "Quickfix List (Trouble)",
      },
    },
  },
  {
    'windwp/nvim-autopairs',
    enabled = not vim.g.vscode,
    event = "InsertEnter",
    opts = {}
  },
  {
    'echasnovski/mini.icons',
    version = false,
    enabled = not vim.g.vscode,
    config = function()
      require('mini.icons').setup()
    end,
  },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    enabled = not vim.g.vscode,
    dependencies = { 'echasnovski/mini.icons' },
    opts = {
    },
    keys = {
      {
        "<leader>?",
        function()
          require("which-key").show({ global = false })
        end,
        desc = "Buffer Local Keymaps (which-key)",
      },
    },
  },
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    enabled = not vim.g.vscode,
    config = function()
      require('lualine').setup({
        sections = {
          lualine_x = {
            {
              require("noice").api.status.message.get_hl,
              cond = require("noice").api.status.message.has,
            },
            {
              require("noice").api.status.command.get,
              cond = require("noice").api.status.command.has,
              color = { fg = "#ff9e64" },
            },
            {
              require("noice").api.status.mode.get,
              cond = require("noice").api.status.mode.has,
              color = { fg = "#ff9e64" },
            },
            {
              require("noice").api.status.search.get,
              cond = require("noice").api.status.search.has,
              color = { fg = "#ff9e64" },
            },
          },
        },
      })
    end,
  },
  {
    "hrsh7th/vim-vsnip",
    dependencies = {
      "hrsh7th/vim-vsnip-integ",
    },
    enabled = not vim.g.vscode,
    config = function()
    end,
  },
  {
    "hrsh7th/cmp-vsnip",
    enabled = not vim.g.vscode,
    config = function()
    end
  },
  {
    "hrsh7th/cmp-nvim-lsp",
    enabled = not vim.g.vscode,
    config = function()
    end
  },
  {
    "hrsh7th/cmp-path",
    enabled = not vim.g.vscode,
    config = function()
    end
  },
  {
    "hrsh7th/cmp-cmdline",
    enabled = not vim.g.vscode,
    config = function()
    end
  },
  {
    "hrsh7th/cmp-buffer",
    enabled = not vim.g.vscode,
    config = function()
    end
  },
  {
    "hrsh7th/nvim-cmp",
    enabled = not vim.g.vscode,
    config = function()
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
      cmp.setup.cmdline({ '/', '?' }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = 'buffer' }
        }
      })
      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = 'path' }
        }, {
          { name = 'cmdline' }
        }),
        matching = { disallow_symbol_nonprefix_matching = false }
      })
      local capabilities = require('cmp_nvim_lsp').default_capabilities()
    end
  },
  {
    "williamboman/mason.nvim",
    enabled = not vim.g.vscode,
    config = function()
      require("mason").setup()
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    enabled = not vim.g.vscode,
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = { "lua_ls", "rust_analyzer", "clojure_lsp" },
        automatic_installation = true,
      })
    end,
  },
  {
    "neovim/nvim-lspconfig",
    enabled = not vim.g.vscode,
    config = function()
      local lspconfig = require('lspconfig')
      local capabilities = require('cmp_nvim_lsp').default_capabilities()

      lspconfig.lua_ls.setup {
        capabilities = capabilities,
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
      lspconfig.rust_analyzer.setup({
        capabilities = capabilities,
      })
      lspconfig.clojure_lsp.setup({
        capabilities = capabilities,
      })
      lspconfig.nil_ls.setup({
        capabilities = capabilities,
      })
      lspconfig.jsonls.setup({
        capabilities = capabilities,
      })
      lspconfig.bashls.setup({
        capabilities = capabilities,
      })
      lspconfig.pyright.setup({
        capabilities = capabilities,
      })
      lspconfig.cssls.setup({
        capabilities = capabilities,
      })
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
    -- ft = { "clojure", "scheme", "lisp" },
    config = function()
    end,
  },
}
