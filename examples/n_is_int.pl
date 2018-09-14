:- consult('../lib/vimscript.pl').
:- use_module('../lib/vimscript.pl').

main :- eval(file([
    let(ident("","n") @ [1,5],=,number("42") @ [1,9]) @ [1,1],
    echo([ident("","n") @ [2,6]]) @ [2,1]
    ]) @ [1,1], _, _), halt.

:- main; halt(1).

