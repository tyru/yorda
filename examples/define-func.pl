:- consult('../lib/vimscript.pl').
:- use_module('../lib/vimscript.pl').

main :- eval(file([
    function(ident("s","id") @ [1,11],[ident("","x") @ [1,16]],[
      return(ident("a","x") @ [2,10]) @ [2,3]
    ]) @ [1,1]
    ]) @ [1,1], _, _), halt.

:- main; halt(1).

