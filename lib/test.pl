:- consult(vimscript).
:- use_module(vimscript).

:- begin_tests(error).
  test(no_such_term) :- not(eval([], foobar, [], _)).
  test(not_equal) :- not(eval([], tInt(42), [], tInt(999))).
:- end_tests(error).

:- begin_tests(primitive_types).
  test(tAny, all(R = [tAny @ Pos])) :-
    eval([], tAny @ Pos, [], R).
  test(tVoid, all(R = [tVoid @ Pos])) :-
    eval([], tVoid @ Pos, [], R).
  test(tInt, all(R = [tInt(_) @ Pos])) :-
    eval([], tInt(_) @ Pos, [], R).
  test(tFloat, all(R = [tFloat(_) @ Pos])) :-
    eval([], tFloat(_) @ Pos, [], R).
  test(tString, all(R = [tString(_) @ Pos])) :-
    eval([], tString(_) @ Pos, [], R).
  test(tList, all(R = [tList(_) @ Pos])) :-
    eval([], tList(_) @ Pos, [], R).
  test(tDict, all(R = [tDict(_, _) @ Pos])) :-
    eval([], tDict(_, _) @ Pos, [], R).
  test(tTuple0, all(R = [tTuple @ Pos])) :-
    eval([], tTuple @ Pos, [], R).
  test(tTuple1, all(R = [tTuple(_) @ Pos])) :-
    eval([], tTuple(_) @ Pos, [], R).
  test(tTuple2, all(R = [tTuple(_, _) @ Pos])) :-
    eval([], tTuple(_, _) @ Pos, [], R).
  test(tTuple3, all(R = [tTuple(_, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _) @ Pos, [], R).
  test(tTuple4, all(R = [tTuple(_, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _) @ Pos, [], R).
  test(tTuple5, all(R = [tTuple(_, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _) @ Pos, [], R).
  test(tTuple6, all(R = [tTuple(_, _, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _, _) @ Pos, [], R).
  test(tTuple7, all(R = [tTuple(_, _, _, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _, _, _) @ Pos, [], R).
  test(tTuple8, all(R = [tTuple(_, _, _, _, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _, _, _, _) @ Pos, [], R).
  test(tTuple9, all(R = [tTuple(_, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _, _, _, _, _) @ Pos, [], R).
  test(tTuple10, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _, _, _, _, _, _) @ Pos, [], R).
  test(tTuple11, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _, _, _, _, _, _, _) @ Pos, [], R).
  test(tTuple12, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _, _, _, _, _, _, _, _) @ Pos, [], R).
  test(tTuple13, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, [], R).
  test(tTuple14, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, [], R).
  test(tTuple15, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, [], R).
  test(tTuple16, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, [], R).
  test(tTuple17, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, [], R).
  test(tTuple18, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, [], R).
  test(tTuple19, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, [], R).
  test(tTuple20, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval([], tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, [], R).
:- end_tests(primitive_types).

:- begin_tests(n_is_int).
  % let n = 42
  % echo n
  test(n_is_int, all(T = [tInt(42) @ [1, 9]])) :-
    eval([], file([
      let(ident("g:n",T) @ [1,5], =, tInt(42) @ [1,9]) @ [1,1],
      echo([ident("g:n",T) @ [2,6]]) @ [2,1]
    ]) @ FilePos, RetEnv, tVoid @ FilePos),
    RetEnv = [ident("g:n", tInt(42)@[1, 9]) @ [1, 5]].
:- end_tests(n_is_int).

:- run_tests.
:- halt.
