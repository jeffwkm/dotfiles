EscMaintainPosition = function()
  InputSpecial("<esc>")
  local col = vim.api.nvim_win_get_cursor(0)[2]
  if col ~= 0 then
    InputSpecial("<right>")
  end
end

Jump_0_or_IndentStart = function()
  local saved_position = vim.api.nvim_win_get_cursor(0)
  local col = saved_position[2]
  vim.api.nvim_command("normal! ^")
  local first_non_blank_col = vim.api.nvim_win_get_cursor(0)[2]
  vim.api.nvim_win_set_cursor(0, saved_position)

  if col == first_non_blank_col then
    vim.api.nvim_command("normal! 0")
  else
    vim.api.nvim_command("normal! ^")
  end
end

BackspaceInNormalMode = function()
  local col = vim.api.nvim_win_get_cursor(0)[2]
  if col == 0 then
    vim.api.nvim_feedkeys("i", "n", false)
    InputSpecial("<bs>")
    InputSpecial("<right>")
    InputSpecial("<esc>")
  else
    InputSpecial("<left>x")
  end
end

EnterInNormalMode = function()
  local saved_position = vim.api.nvim_win_get_cursor(0)
  local col = saved_position[2]
  vim.api.nvim_command("normal! $")
  local eol = vim.api.nvim_win_get_cursor(0)[2]
  vim.api.nvim_win_set_cursor(0, saved_position)
  if col == eol then
    vim.api.nvim_feedkeys("a", "n", false)
  else
    vim.api.nvim_feedkeys("i", "n", false)
  end
  InputSpecial("<cr>")
  InputSpecial("<right>")
  InputSpecial("<esc>")
end

-- K("i", "<Esc>", EscMaintainPosition, Opts) -- prevent cursor from shifting left when exiting insert mode
Knv("0", Jump_0_or_IndentStart)   -- bind 0 to toggle between ^ and 0
Ko("0", "normal! 0")              -- use standard 0 in object mode
Kn("<bs>", BackspaceInNormalMode) -- delete with backspace in normal mode
Kn("<cr>", EnterInNormalMode)     -- insert linebreak with enter in normal mode
Kn("<Esc>", "<Esc>:noh<CR>")      -- removes highlighting after escaping vim search
-- Knv("<leader>y", '"+y')                 -- yank to system clipboard
-- Knv("<leader>p", '"+p')                 -- paste from system clipboard

if vim.g.vscode then
  K({ "x", "n", "o" }, "gc", "<Plug>VSCodeCommentary", { silent = true })
  K("n", "gcc", "<Plug>VSCodeCommentaryLine", { silent = true })

  MapVscLeader(",", "workbench.action.quickOpen")
  MapVscLeader("<enter>", "workbench.action.showCommands")

  MapVscLeader("r", "workbench.action.reloadWindow")

  MapVscLeader("/", "actions.find")
  MapVscLeader("<Leader>/", "workbench.action.findInFiles")

  MapVscLeader("fs", "workbench.action.files.save")
  MapVscLeader("fS", "workbench.action.files.saveAll")

  MapVscLeader("=", "editor.action.fontZoomIn")
  MapVscLeader("-", "editor.action.fontZoomOut")
  MapVscLeader("0", "editor.action.fontZoomReset")

  MapVscLeader("v'", "workbench.action.togglePanel")
  MapVscLeader("v;", "workbench.action.toggleMaximizedPanel")
  MapVscLeader("v[", "workbench.action.toggleSidebarVisibility")
  MapVscLeader("v]", "workbench.action.toggleAuxiliaryBar")
  MapVscLeader("vt", "workbench.action.terminal.toggleTerminal")
  MapVscLeader("vo", "workbench.action.output.toggleOutput")
  MapVscLeader("vp", "workbench.actions.view.problems")

  MapVscLeader(";", "workbench.action.previousEditor")
  MapVscLeader("'", "workbench.action.nextEditor")

  MapVscLeader("o", "workbench.action.focusNextGroup")
  MapVscLeader("wk", "workbench.action.closeActiveEditor")
  MapVscLeader("wK", "workbench.action.closeGroup")

  MapVscLeader("nn", "workbench.action.files.newFile")

  MapVscLeader("ss", "workbench.action.openSettingsJson")
  MapVscLeader("sS", "workbench.action.openRawDefaultSettings")
  MapVscLeader("sk", "workbench.action.openGlobalKeybindingsFile")
  MapVscLeader("sK", "workbench.action.openDefaultKeybindingsFile")
end
