xmap gc  <Plug>VSCodeCommentary
nmap gc  <Plug>VSCodeCommentary
omap gc  <Plug>VSCodeCommentary
nmap gcc <Plug>VSCodeCommentaryLine

nnoremap <Leader><enter> <Cmd>call VSCodeNotifyVisual('workbench.action.showCommands', 0)<CR>
nnoremap <Leader>, <Cmd>call VSCodeNotifyVisual('workbench.action.quickOpen', 1)<CR>
nnoremap <Leader>r <Cmd>call VSCodeNotify('workbench.action.reloadWindow')<CR>
nnoremap <Leader>fs <Cmd>call VSCodeNotify('workbench.action.files.save')<CR>
nnoremap <Leader>fS <Cmd>call VSCodeNotify('workbench.action.files.saveAll')<CR>
nnoremap <Leader>wk <Cmd>call VSCodeNotify('workbench.action.closeActiveEditor')<CR>
nnoremap <Leader><Leader>tM <Cmd>call VSCodeNotify('workbench.action.toggleMenuBar')<CR>
nnoremap <Leader>h <Cmd>call VSCodeNotify('workbench.action.previousEditor')<CR>
nnoremap <Leader>l <Cmd>call VSCodeNotify('workbench.action.nextEditor')<CR>
nnoremap <Leader>= <Cmd>call VSCodeNotify('editor.action.fontZoomIn')<CR>
nnoremap <Leader>- <Cmd>call VSCodeNotify('editor.action.fontZoomOut')<CR>
nnoremap <Leader>0 <Cmd>call VSCodeNotify('editor.action.fontZoomReset')<CR>
nnoremap <Leader>vp <Cmd>call VSCodeNotify('workbench.action.togglePanel')<CR>
nnoremap <Leader>vs <Cmd>call VSCodeNotify('workbench.action.toggleSidebarVisibility')<CR>
nnoremap <Leader>vS <Cmd>call VSCodeNotify('workbench.action.toggleAuxiliaryBar')<CR>
nnoremap <Leader>vt <Cmd>call VSCodeNotify('workbench.action.terminal.toggleTerminal')<CR>
nnoremap <Leader>vo <Cmd>call VSCodeNotify('workbench.action.output.toggleOutput')<CR>
nnoremap <Leader>vp <Cmd>call VSCodeNotify('workbench.actions.view.problems')<CR>
nnoremap <Leader>vT <Cmd>call VSCodeNotify('workbench.action.toggleTabsVisibility')<CR>
nnoremap <Leader>oss <Cmd>call VSCodeNotify('workbench.action.openGlobalSettings')<CR>
nnoremap <Leader>okk <Cmd>call VSCodeNotify('workbench.action.openGlobalKeybindings')<CR>
nnoremap <Leader>osj <Cmd>call VSCodeNotify('workbench.action.openSettingsJson')<CR>
nnoremap <Leader>okj <Cmd>call VSCodeNotify('workbench.action.openGlobalKeybindingsFile')<CR>

" runtime macros/sandwich/keymap/surround.vim
