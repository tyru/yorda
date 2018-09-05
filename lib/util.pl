:- module(vimscript, [
  type_name/2,
  stringify/2,
]).

% ===================== Utilities =====================

% for variable (e.g. type_name(T, N))
type_name(unknown, N) :- N = "Unknown", !.
type_name(tAny, N) :- N = "Any", !.
type_name(tVoid, N) :- N = "Void", !.
type_name(tInt(_), N) :- N = "Int", !.
type_name(tFloat(_), N) :- N = "Float", !.
type_name(tString(_), N) :- N = "String", !.
type_name(tList(_), N) :- N = "List", !.
type_name(tDict(_), N) :- N = "Dict", !.
type_name(tTuple, N) :- N = "[]", !.
type_name(tTuple(T1), N) :-
  type_name(T1, T1Name),
  concat_atom(["[", T1Name, "]"], "", N),
  !.
type_name(tTuple(T1, T2), N) :-
  type_name(T1, T1Name),
  type_name(T2, T2Name),
  concat_atom(["[", T1Name, ", ", T2Name, "]"], "", N),
  !.
type_name(tTuple(T1, T2, T3), N) :-
  type_name(T1, T1Name),
  type_name(T2, T2Name),
  type_name(T3, T3Name),
  concat_atom(["[", T1Name, ", ", T2Name, ", ", T3Name, "]"], "", N),
  !.
% TODO more tTuple rules

% for wrong prim (e.g. type_name("foo", N))
type_name(S, N) :-
  string(S),
  N = S,
  !.
% for wrong prim (e.g. type_name(foo, N))
type_name(Atom, N) :- atom_string(Atom, N).

error(Expr, Msg, Err) :- prim(Err) = prim(tError(Expr, Msg)).
error_string(Expr, Expected, Got, Err) :-
  stringify(Expected, E),
  stringify(Got, G),
  concat_atom(["expected ", E, " but got ", G], Atom),
  atom_string(Atom, Msg),
  error(Expr, Msg, Err).

stringify(S1, S2) :-
  string(S1),
  S2 = S1.
stringify(T, S) :-
  prim(T),
  type_name(T, S).
stringify(V, S) :-
  term_string(V, S).
stringify(L, S) :-
  is_list(L),
  stringify_list(L, "", S).
stringify_list([V|Rest], S, Result) :-
  stringify(V, VS),
  concat_atom([S, VS], "", Added),
  stringify_list(Rest, Added, Result).
stringify_list([], S, Result) :- Result = S.
