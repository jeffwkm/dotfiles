function! NvimUiModeChanged(mode)
  "  echom "NvimUiModeChanged: " . a:mode
  if a:mode == "i"
    call VSCodeNotify('nvim-theme.insert')
  elseif a:mode == "r" || a:mode == "R"
    call VSCodeNotify('nvim-theme.replace')
  elseif a:mode == "v" || a:mode == "V"
    call VSCodeNotify('nvim-theme.visual')
  elseif a:mode == "n"
    call VSCodeNotify('nvim-theme.normal')
  endif
endfunction

augroup CursorLineNrColorSwap
  autocmd!
  autocmd ModeChanged *:[irRvVn\x06] call NvimUiModeChanged(v:event['new_mode'])
augroup END
