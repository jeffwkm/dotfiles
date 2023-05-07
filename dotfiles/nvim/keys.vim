" set leader key to space
let g:mapleader = " "

function! BackspaceInInsertMode() abort
  if col(".") == 1
    call nvim_feedkeys("\<Insert>","n",0)
    call nvim_feedkeys("\<BS>\<Right>","n",0)
    call nvim_feedkeys("\<Esc>","n",0)
  else
    call nvim_feedkeys("\<Left>x","n",0)
  endif
endfunction

function! EnterInInsertMode() abort
  if col(".") == col("$") - 1
    call nvim_feedkeys("a", "n", 0)
  else
    call nvim_feedkeys("i", "n", 0)
  endif
  call nvim_feedkeys("\<CR>", "n", 0)
  call nvim_feedkeys("\<Esc>", "n", 0)
endfunction

nnoremap <BS> :call BackspaceInInsertMode()<CR>
nnoremap <CR> :call EnterInInsertMode()<CR>

xmap gc  <Plug>VSCodeCommentary
nmap gc  <Plug>VSCodeCommentary
omap gc  <Plug>VSCodeCommentary
nmap gcc <Plug>VSCodeCommentaryLine

nnoremap <Leader>s <Plug>(easymotion-s2)
onoremap <Leader>s <Plug>(easymotion-s2)
nnoremap t <Plug>(easymotion-tl)
onoremap t <Plug>(easymotion-tl)
nnoremap T <Plug>(easymotion-Tl)
onoremap T <Plug>(easymotion-Tl)
nnoremap <Leader>j <Plug>(easymotion-j)
onoremap <Leader>j <Plug>(easymotion-j)
nnoremap <Leader>k <Plug>(easymotion-k)
onoremap <Leader>k <Plug>(easymotion-k)

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

"  runtime macros/sandwich/keymap/surround.vim
