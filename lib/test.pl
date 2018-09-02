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
  test(tDict, all(R = [tDict(_) @ Pos])) :-
    eval([], tDict(_) @ Pos, [], R).
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

:- begin_tests(expr).
  % abs(42) == 42
  test(call_abs_int) :-
    eval([],
      call(ident("g","abs",0) @ nopos, [tInt(42) @ nopos]) @ [1,1],
      [],
      tInt(42) @ [1,1]).
  % abs(12.34) == 12.34
  test(call_abs_float) :-
    eval([],
      call(ident("g","abs",0) @ nopos, [tFloat(12.34) @ nopos]) @ [1,1],
      [],
      tFloat(12.34) @ [1,1]).
  % abs(<tInt>) != <tFloat>
  test(call_abs_fail1) :-
    not(eval([],
      call(ident("g","abs",0) @ nopos, [tInt(42) @ nopos]) @ [1,1],
      [],
      tFloat(_) @ [1,1])).
  % abs(<tFloat>) != <tInt>
  test(call_abs_fail2) :-
    not(eval([],
      call(ident("g","abs",0) @ nopos, [tFloat(12.34) @ nopos]) @ [1,1],
      [],
      tInt(_) @ [1,1])).
  % acos(0.0) == 1.570796
  test(call_acos) :-
    eval([],
      call(ident("g","acos",0) @ nopos, [tFloat(0) @ nopos]) @ [1,1],
      [],
      tFloat(1.570796) @ [1,1]).
  % add([], 42) == [42]
  test(call_add) :-
    eval([],
      call(ident("g","add",0) @ nopos, [tList([]) @ nopos, tInt(42) @ [1,5]]) @ [1,1],
      [],
      tList([tInt(42) @ [1,5]]) @ [1,1]).
  % and(1, 1) == 1
  test(call_and) :-
    eval([],
      call(ident("g","and",0) @ nopos, [tInt(1) @ nopos, tInt(1) @ nopos]) @ [1,1],
      [],
      tInt(1) @ [1,1]).
  % append(1, "foo") == 0
  test(call_append) :-
    eval([],
      call(ident("g","append",0) @ nopos, [tInt(1) @ nopos, tString("foo") @ nopos]) @ [1,1],
      [],
      tInt(0) @ [1,1]).
  % append(".", "foo") == 0
  test(call_append) :-
    eval([],
      call(ident("g","append",0) @ nopos, [tString(".") @ nopos, tString("foo") @ nopos]) @ [1,1],
      [],
      tInt(0) @ [1,1]).
  % append(1, []) == 0
  test(call_append) :-
    eval([],
      call(ident("g","append",0) @ nopos, [tInt(1) @ nopos, tList([]) @ nopos]) @ [1,1],
      [],
      tInt(0) @ [1,1]).
  % append(".", []) == 0
  test(call_append) :-
    eval([],
      call(ident("g","append",0) @ nopos, [tString(".") @ nopos, tList([]) @ nopos]) @ [1,1],
      [],
      tInt(0) @ [1,1]).
  % has("eval") == 1
  test(call_has) :-
    eval([],
      call(ident("g","has",0) @ nopos, [tString("eval") @ nopos]) @ [1,1],
      [],
      tInt(1) @ [1,1]).
:- end_tests(expr).

:- begin_tests(excmds).
  % let n = 42
  % echo n
  test(n_is_int, all(RetEnv = [[ident("g", "n", 0) @ [1, 5] => tInt(42) @ [1, 9]]])) :-
    eval([], file([
      let(ident("g","n",0) @ [1,5], =, tInt(42) @ [1,9]) @ nopos,
      echo([ident("g","n",0) @ nopos]) @ nopos
    ]) @ [1,1], RetEnv, tVoid @ [1,1]).

  % let has = 42
  % echo has("eval")
  % echo has
  test(func_and_var_are_different_namespaces, all(
    RetEnv = [[ident("g", "has", 0) @ [1, 5] => tInt(42) @ [1, 11]]]
  )) :-
    eval([], file([
      let(ident("g","has",0) @ [1,5], =, tInt(42) @ [1,11]) @ nopos,
      echo([call(ident("g","has",0) @ nopos, [tString("eval") @ nopos]) @ nopos]) @ nopos,
      echo([ident("g","has",0) @ nopos]) @ nopos
    ]) @ [1,1], RetEnv, tVoid @ [1,1]).
:- end_tests(excmds).

:- run_tests.
:- halt.
