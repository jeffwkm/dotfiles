" set leader key to space
let g:mapleader = " "

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

onoremap <Leader>s <Plug>(easymotion-s2)
nnoremap <Leader>s <Plug>(easymotion-s2)
nnoremap t <Plug>(easymotion-tl)
onoremap t <Plug>(easymotion-tl)
nnoremap T <Plug>(easymotion-Tl)
onoremap T <Plug>(easymotion-Tl)

nnoremap <Leader>j <Plug>(easymotion-j)
nnoremap <Leader>k <Plug>(easymotion-k)

"  runtime macros/sandwich/keymap/surround.vim
