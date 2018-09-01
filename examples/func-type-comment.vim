" @param {Dict} wg
" @param {Int} index
" @returns {tInt}
function! s:_notify_done(wg, index, value) abort
  let a:wg.results[a:index] = a:value
  let a:wg.remaining -= 1
  if a:wg.remaining == 0
    call a:wg.resolve(a:wg.results)
  endif
endfunction
