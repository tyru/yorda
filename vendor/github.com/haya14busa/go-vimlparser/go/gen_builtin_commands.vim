" Generate go/builtin_commands.go

let s:lines = []

function! s:put(line) abort
  call add(s:lines, a:line)
endfunction

call s:put('// Code generated by gen_builtin_commands.vim')
call s:put('// source: autoload/vimlparser.vim')
call s:put('// DO NOT EDIT!')
call s:put('')
call s:put('package vimlparser')
call s:put('')

call s:put('var neovim_additional_commands = []*Cmd{')
for s:cmd in vimlparser#import().VimLParser.neovim_additional_commands
  call s:put("\t" . printf('{flags: "%s", minlen: %s, name: "%s", parser: "%s" },',
  \ s:cmd.flags, s:cmd.minlen, s:cmd.name, s:cmd.parser))
endfor
call s:put('}')

call s:put('var neovim_removed_commands = []*Cmd{')
for s:cmd in vimlparser#import().VimLParser.neovim_removed_commands
  call s:put("\t" . printf('{flags: "%s", minlen: %s, name: "%s", parser: "%s" },',
  \ s:cmd.flags, s:cmd.minlen, s:cmd.name, s:cmd.parser))
endfor
call s:put('}')

call s:put('var builtin_commands = []*Cmd{')
for s:cmd in vimlparser#import().VimLParser.builtin_commands
  call s:put("\t" . printf('{flags: "%s", minlen: %s, name: "%s", parser: "%s" },',
  \ s:cmd.flags, s:cmd.minlen, s:cmd.name, s:cmd.parser))
endfor
call s:put('}')

call writefile(s:lines, expand('<sfile>:p:h') . '/builtin_commands.go')
