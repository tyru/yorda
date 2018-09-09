:- consult('../lib/vimscript.pl').
:- use_module('../lib/vimscript.pl').

main :- new_env(Env), eval(Env, file([
    echo([call(ident("","call") @ [1,6],[call(ident("","function") @ [1,11],[tString("has") @ [1,20]]) @ [1,19],tList([tString("eval") @ [1,29]]) @ [1,28]]) @ [1,10]]) @ [1,1],
    let(ident("","F") @ [3,5],=,call(ident("","function") @ [3,9],[tString("has") @ [3,18]]) @ [3,17]) @ [3,1],
    echo([call(ident("","call") @ [4,6],[ident("","F") @ [4,11],tList([tString("eval") @ [4,15]]) @ [4,14]]) @ [4,10]]) @ [4,1],
    let(ident("g","F") @ [6,5],=,call(ident("","function") @ [6,11],[tString("has") @ [6,20]]) @ [6,19]) @ [6,1],
    echo([call(ident("","call") @ [7,6],[ident("g","F") @ [7,11],tList([tString("eval") @ [7,17]]) @ [7,16]]) @ [7,10]]) @ [7,1]
    ]) @ [1,1], _, _), halt.

:- main; halt(1).

