function! Cond(Cond, ...)
  let opts = get(a:000, 0, {})
  return a:Cond ? opts : extend(opts, { 'on': [], 'for': [] })
endfunction

let g:EasyMotion_do_mapping = 0
let g:EasyMotion_startofline = 0
let g:EasyMotion_verbose = 0
let g:EasyMotion_prompt = ''

call plug#begin('~/.config/nvim/data/plugged')
Plug 'tpope/vim-sensible'
Plug 'easymotion/vim-easymotion', Cond(!exists('g:vscode'))
Plug 'asvetliakov/vim-easymotion', Cond(exists('g:vscode'), { 'as': 'vsc-easymotion' })
Plug 'tpope/vim-commentary', Cond(!exists('g:vscode'))
Plug 'machakann/vim-sandwich'
call plug#end()
" Plug 'kovisoft/paredit'
" Plug 'justinmk/vim-sneak'

augroup easymotion_config
  autocmd!
  autocmd VimEnter * let g:EasyMotion_do_mapping = 0
augroup END

let g:logging_level = 'warn'

set hlsearch
set nonumber

if exists('g:vscode')
    "" quick-scope
    highlight QuickScopePrimary guifg='#afff5f' gui=underline ctermfg=155 cterm=underline
    highlight QuickScopeSecondary guifg='#5fffff' gui=underline ctermfg=81 cterm=underline
    "" vim-sandwidth
    highlight OperatorSandwichBuns guifg='#aa91a0' gui=underline ctermfg=172 cterm=underline
    highlight OperatorSandwichChange guifg='#edc41f' gui=underline ctermfg='yellow' cterm=underline
    highlight OperatorSandwichAdd guibg='#b1fa87' gui=none ctermbg='green' cterm=none
    highlight OperatorSandwichDelete guibg='#cf5963' gui=none ctermbg='red' cterm=none

    " load file keys.vim from current directory
    runtime! keys.vim
else
    " ordinary Neovim
endif
