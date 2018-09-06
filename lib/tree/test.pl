:- consult(tree).
:- use_module(tree).

:- begin_tests(tree).

  test(new_tree1) :-
    new_tree(top, top++[]).
  test(new_tree2) :-
    new_tree(node++[], node++[]).

  test(split_tree1, Part = a++[b++[], c++[]], Whole = Part) :-
    append_tree(a++[b++[], c++[]], [], Part, Whole :: xxx).
  test(split_tree2, Part = b++[], Whole = a++[xxx, c++[]]) :-
    append_tree(a++[b++[], c++[]], [0], Part, Whole :: xxx).
  test(split_tree3, Part = c++[], Whole = a++[b++[], xxx]) :-
    append_tree(a++[b++[], c++[]], [1], Part, Whole :: xxx).
  test(split_tree4) :-
    not(append_tree(a++[], [0], _, _)).
  test(split_tree5) :-
    not(append_tree(a++[], [1], _, _)).

  test(append_tree1, Ans = a++[b++[]]) :-
    append_tree(a++[], [], b, Ans).
  test(append_tree2, Ans = a++[b++[]]) :-
    append_tree(a++[], [], b++[], Ans).
  test(append_tree3, Ans = a++[b++[c++[]]]) :-
    append_tree(a++[], [], b++[c++[]], Ans).
  test(append_tree4, Ans = a++[b++[], c++[]]) :-
    append_tree(a++[b++[]], [], c++[], Ans).
  test(append_tree5, Ans = a++[b++[], c++[], d++[]]) :-
    append_tree(a++[b++[], c++[]], [], d++[], Ans).
  test(append_tree6, Ans = a++[b++[c++[]]]) :-
    append_tree(a++[b++[]], [0], c++[], Ans).
  test(append_tree7, Ans = a++[b++[], c++[d++[]]]) :-
    append_tree(a++[b++[], c++[]], [1], d++[], Ans).
  test(append_tree8, Ans = a++[b++[], c++[d++[e++[]]]]) :-
    append_tree(a++[b++[], c++[d++[]]], [1, 0], e++[], Ans).
  test(append_tree9, Ans = a++[b++[d++[], e++[f++[]]], c++[]]) :-
    append_tree(a++[b++[d++[], e++[]], c++[]], [0, 1], f++[], Ans).
  test(append_tree10, Ans = a++[b++[d++[e++[]]], c++[]]) :-
    append_tree(a++[b++[d++[]], c++[]], [0, 0], e++[], Ans).
  test(append_tree11, Ans = a++[b++[], c++[d++[], e++[f++[]]]]) :-
    append_tree(a++[b++[], c++[d++[], e++[]]], [1, 1], f++[], Ans).

  test(prepend_tree1, Ans = a++[b++[]]) :-
    prepend_tree(a++[], [], b, Ans).
  test(prepend_tree2, Ans = a++[b++[]]) :-
    prepend_tree(a++[], [], b++[], Ans).
  test(prepend_tree3, Ans = a++[b++[c++[]]]) :-
    prepend_tree(a++[], [], b++[c++[]], Ans).
  test(prepend_tree4, Ans = a++[c++[], b++[]]) :-
    prepend_tree(a++[b++[]], [], c++[], Ans).
  test(prepend_tree5, Ans = a++[d++[], b++[], c++[]]) :-
    prepend_tree(a++[b++[], c++[]], [], d++[], Ans).
  test(prepend_tree6, Ans = a++[b++[c++[]]]) :-
    prepend_tree(a++[b++[]], [0], c++[], Ans).
  test(prepend_tree7, Ans = a++[b++[], c++[d++[]]]) :-
    prepend_tree(a++[b++[], c++[]], [1], d++[], Ans).
  test(prepend_tree8, Ans = a++[b++[], c++[d++[e++[]]]]) :-
    prepend_tree(a++[b++[], c++[d++[]]], [1, 0], e++[], Ans).
  test(prepend_tree9, Ans = a++[b++[d++[], e++[f++[]]], c++[]]) :-
    prepend_tree(a++[b++[d++[], e++[]], c++[]], [0, 1], f++[], Ans).
  test(prepend_tree10, Ans = a++[b++[d++[e++[]]], c++[]]) :-
    prepend_tree(a++[b++[d++[]], c++[]], [0, 0], e++[], Ans).
  test(prepend_tree11, Ans = a++[b++[], c++[d++[], e++[f++[]]]]) :-
    prepend_tree(a++[b++[], c++[d++[], e++[]]], [1, 1], f++[], Ans).

  test(replace_tree1, Ans = b++[]) :-
    replace_tree(a++[], [], b, Ans).
  test(replace_tree2, Ans = b++[]) :-
    replace_tree(a++[], [], b++[], Ans).
  test(replace_tree3, Ans = a++[c++[]]) :-
    replace_tree(a++[b++[]], [0], c++[], Ans).
  test(replace_tree4, Ans = a++[b++[], d++[]]) :-
    replace_tree(a++[b++[], c++[]], [1], d++[], Ans).
  test(replace_tree5, Ans = a++[b++[], c++[e++[]]]) :-
    replace_tree(a++[b++[], c++[d++[]]], [1, 0], e++[], Ans).
  test(replace_tree6, Ans = a++[b++[d++[], f++[]], c++[]]) :-
    replace_tree(a++[b++[d++[], e++[]], c++[]], [0, 1], f++[], Ans).
  test(replace_tree7, Ans = a++[b++[e++[]], c++[]]) :-
    replace_tree(a++[b++[d++[]], c++[]], [0, 0], e++[], Ans).
  test(replace_tree8, Ans = a++[b++[], c++[d++[], f++[]]]) :-
    replace_tree(a++[b++[], c++[d++[], e++[]]], [1, 1], f++[], Ans).

  test(get_node1) :-
    get_node(a++[], [], a++[]).
  test(get_node2) :-
    get_node(a++[b++[c++[]]], [0], b++[c++[]]).
  test(get_node3) :-
    get_node(a++[b++[c++[]]], [0,0], c++[]).
  test(get_node4) :-
    get_node(a++[b++[c++[]], d++[e++[]]], [1], d++[e++[]]).
  test(get_node5) :-
    get_node(a++[b++[c++[]], d++[e++[]]], [1,0], e++[]).

  test(get_item1) :-
    get_item(a++[], [], a).
  test(get_item2) :-
    get_item(a++[b++[c++[]]], [0], b).
  test(get_item3) :-
    get_item(a++[b++[c++[]]], [0,0], c).
  test(get_item4) :-
    get_item(a++[b++[c++[]], d++[e++[]]], [1], d).
  test(get_item5) :-
    get_item(a++[b++[c++[]], d++[e++[]]], [1,0], e).

  test(find_tree1) :-
    find_tree(a++[], =(a++_), [], a++[]).
  test(find_tree2) :-
    \+ find_tree(a++[], =(b++_), _, _).
  test(find_tree3) :-
    find_tree(a++[b++[]], =(b++_), [0], b++[]).
  test(find_tree4) :-
    find_tree(a++[b++[c++[]]], =(b++_), [0], b++[c++[]]).
  test(find_tree5) :-
    find_tree(a++[b++[], c++[]], =(c++_), [1], c++[]).
  test(find_tree6) :-
    find_tree(a++[b++[], c++[d++[]]], =(c++_), [1], c++[d++[]]).
  test(find_tree7) :-
    find_tree(a++[b++[], c++[b++[]]], =(b++_), [0], b++[]).
  test(find_tree8) :-
    find_tree(a++[b++[c++[b++[]]]], =(b++_), [0], b++[c++[b++[]]]).

:- end_tests(tree).

:- run_tests.
:- halt.
