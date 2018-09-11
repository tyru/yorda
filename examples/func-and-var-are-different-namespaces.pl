:- consult('../lib/vimscript.pl').
:- use_module('../lib/vimscript.pl').

main :- new_eval_env(Env), eval(Env, file([
    let(ident("","has") @ [1,5],=,tInt(42) @ [1,11]) @ [1,1],
    echo([call(ident("","has") @ [2,6],[tString("eval") @ [2,10]]) @ [2,9]]) @ [2,1],
    echo([ident("","has") @ [3,6]]) @ [3,1]
    ]) @ [1,1], _, _), halt.

:- main; halt(1).

