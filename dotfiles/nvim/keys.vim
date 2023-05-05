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

nmap s <Plug>(easymotion-s2)

" unmap all text motion operators starting with s

"  nmap <Leader>s <Plug>(easymotion-s2)
"  nmap <Leader>t <Plug>(easymotion-t2)

"  map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
"  map <Leader>h <Plug>(easymotion-linebackward)

"  runtime macros/sandwich/keymap/surround.vim
