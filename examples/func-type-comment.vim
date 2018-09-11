" @param {Dict} dict
" @param {String} key
" @returns {Int}
function! s:f(dict, key) abort
  return a:dict[a:key]
endfunction
