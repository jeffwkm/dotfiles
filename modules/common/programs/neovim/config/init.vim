function! Cond(Cond, ...)
  let opts = get(a:000, 0, {})
  return a:Cond ? opts : extend(opts, { 'on': [], 'for': [] })
endfunction

let g:logging_level = 'warn'
let g:EasyMotion_startofline = 0
let g:EasyMotion_verbose = 0
let g:EasyMotion_show_prompt = 0
let g:EasyMotion_prompt = ''

call plug#begin('~/.config/nvim/data/plugged')
" Plug 'tpope/vim-sensible'
Plug 'easymotion/vim-easymotion'
Plug 'tpope/vim-commentary', Cond(!exists('g:vscode'))
Plug 'machakann/vim-sandwich'
call plug#end()
" Plug 'kovisoft/paredit'
" Plug 'justinmk/vim-sneak'

" Add hook to run before EasyMotion is initialized
" to prevent creating key bindings
augroup easymotion_config
  autocmd!
  autocmd VimEnter * let g:EasyMotion_do_mapping = 0
augroup END

" set leader key to space
let g:mapleader = " "

" Define global variable to prevent triggering hooks when entering insert mode
let g:fake_insert_mode = 0

nnoremap s <Plug>(easymotion-s2)
onoremap s <Plug>(easymotion-s2)
nnoremap f <Plug>(easymotion-f)
onoremap f <Plug>(easymotion-f)
nnoremap F <Plug>(easymotion-F)
onoremap F <Plug>(easymotion-F)
nnoremap t <Plug>(easymotion-tl)
onoremap t <Plug>(easymotion-tl)
nnoremap T <Plug>(easymotion-Tl)
onoremap T <Plug>(easymotion-Tl)
nnoremap gsj <Plug>(easymotion-j)
onoremap gsj <Plug>(easymotion-j)
nnoremap gsk <Plug>(easymotion-k)
onoremap gsk <Plug>(easymotion-k)

function! BackspaceInNormalMode() abort
  let g:fake_insert_mode = 1
  if col(".") == 1
    call nvim_feedkeys("i", "n", 0)
    call nvim_feedkeys("\<BS>", "n", 0)
    call nvim_feedkeys("\<Right>", "n", 0)
    call nvim_feedkeys("\<Esc>", "n", 0)
    "  call feedkeys("\<Left>", "n")
  else
    call nvim_feedkeys("\<Left>x", "n", 0)
  endif
endfunction

function! EnterInNormalMode() abort
  let g:fake_insert_mode = 1
  if col(".") == col("$") - 1
    call nvim_feedkeys("a", "n", 0)
  else
    call nvim_feedkeys("i", "n", 0)
  endif
  call nvim_feedkeys("\<CR>", "n", 0)
  call nvim_feedkeys("\<Right>", "n", 0)
  call nvim_feedkeys("\<Esc>", "n", 0)
endfunction

nnoremap <BS> <Cmd>call BackspaceInNormalMode()<CR>

nnoremap <CR> <Cmd>call EnterInNormalMode()<CR>

set hlsearch
set nonumber

function! IsCursorAtFirstNonBlank() abort
  let saved_position = getcurpos()
  normal! ^
  let first_non_blank_col = col(".")
  call setpos(".", saved_position)
  "  echo 'saved ' . saved_position[2] . ' first ' . first_non_blank_col
  return saved_position[2] == first_non_blank_col
endfunction

function Go0orIndentStart() abort
  if IsCursorAtFirstNonBlank()
    normal! 0
  else
    normal! ^
  endif
endfunction

nnoremap 0 <Cmd>call Go0orIndentStart()<CR>
onoremap 0 normal! 0

function EscInsertExit() abort
  call nvim_feedkeys("\<Esc>", "n", 0)
  if col(".") != 1
    call nvim_feedkeys("\<Right>", "n", 0)
  endif
endfunction

inoremap <Esc> <Cmd>call EscInsertExit()<CR>

" Fix cursor position after leaving insert mode
" TODO: delete? replaced by EscInsertExit()
"
" This is a workaround for the cursor moving left by one
" when insert mode is entered and then exited.
"  augroup VscodeAfterInsert
"    autocmd!
"    "  autocmd InsertLeave * call feedkeys('l', 'n')
"    autocmd InsertLeave * normal! l
"  augroup END

if exists('g:vscode')
    "" quick-scope
    highlight QuickScopePrimary guifg='#afff5f' gui=underline ctermfg=155 cterm=underline
    highlight QuickScopeSecondary guifg='#5fffff' gui=underline ctermfg=81 cterm=underline
    "" vim-sandwidth
    highlight OperatorSandwichBuns guifg='#aa91a0' gui=underline ctermfg=172 cterm=underline
    highlight OperatorSandwichChange guifg='#edc41f' gui=underline ctermfg='yellow' cterm=underline
    highlight OperatorSandwichAdd guibg='#b1fa87' gui=none ctermbg='green' cterm=none
    highlight OperatorSandwichDelete guibg='#cf5963' gui=none ctermbg='red' cterm=none

    runtime! keys.vim
    runtime! ui.vim
else
    " ordinary Neovim
endif
