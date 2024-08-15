K = vim.keymap.set

Opts = { noremap = true, silent = true }

Kn = function(key, command, opts)
  opts = opts or Opts
  K("n", key, command, opts)
end
Ko = function(key, command, opts)
  opts = opts or Opts
  K("o", key, command, opts)
end
Kno = function(key, command, opts)
  opts = opts or Opts
  K({ "n", "o" }, key, command, opts)
end
Knv = function(key, command, opts)
  opts = opts or Opts
  K({ "n", "v" }, key, command, opts)
end
Knov = function(key, command, opts)
  opts = opts or Opts
  K({ "n", "o", "v" }, key, command, opts)
end

-- sends a special key such as <esc> as input
InputSpecial = function(key)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, false, true), 'n', true)
end

MapVscLeader = function(key, cmd, args, modes, opts)
  if vim.g.vscode then
    local modes = modes or { "n", "v" }
    local opts = opts or Opts

    local vim_command
    if args then
      vim_command = "<Cmd>call VSCodeNotify('" .. cmd .. "', " .. args .. ")<CR>"
    else
      vim_command = "<Cmd>call VSCodeNotify('" .. cmd .. "')<CR>"
    end

    K(modes, "<Leader>" .. key, vim_command, opts)
  end
end

MapVscLeaderN = function(key, cmd, args, opts)
  MapVscLeader(key, cmd, args, { "n" }, opts)
end
