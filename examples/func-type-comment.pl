:- consult('../lib/vimscript.pl').
:- use_module('../lib/vimscript.pl').

main :- new_env(Env), eval(Env, file([
    comment(" @param {Dict} dict") @ [1,1],
    comment(" @param {String} key") @ [2,1],
    comment(" @returns {Int}") @ [3,1],
    function(ident("s","f") @ [4,11],[ident("","dict") @ [4,15],ident("","key") @ [4,21]],[
      return(subscript(ident("a","dict") @ [5,10],ident("a","key") @ [5,17]) @ [5,16]) @ [5,3]
    ]) @ [4,1]
    ]) @ [1,1], _, _), halt.

:- main; halt(1).

