:- consult(tree).
:- use_module(tree).

:- begin_tests(tree).

  test(new_tree1) :-
    new_tree(top, top++[]).
  test(new_tree2) :-
    new_tree(node++[], node++[]).

  test(split_tree1, Part = a++[b++[], c++[]], Whole = Part) :-
    split_tree(a++[b++[], c++[]], [], Part, Whole :: xxx).
  test(split_tree2, Part = b++[], Whole = a++[xxx, c++[]]) :-
    split_tree(a++[b++[], c++[]], [0], Part, Whole :: xxx).
  test(split_tree3, Part = c++[], Whole = a++[b++[], xxx]) :-
    split_tree(a++[b++[], c++[]], [1], Part, Whole :: xxx).
  test(split_tree4) :-
    \+ split_tree(a++[], [0], _, _).
  test(split_tree5) :-
    \+ split_tree(a++[], [1], _, _).

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

  test(traverse_tree1) :-
    Tree = a++[],
    findall((Path, Node), traverse_tree(Tree, Path, Node), Results),
    Results = [([], a++[])].
  test(traverse_tree2) :-
    Tree = a++[b++[]],
    findall((Path, Node), traverse_tree(Tree, Path, Node), Results),
    Results = [([], a++[b++[]]), ([0], b++[])].
  test(traverse_tree3) :-
    Tree = a++[b++[c++[]]],
    findall((Path, Node), traverse_tree(Tree, Path, Node), Results),
    Results = [([], a++[b++[c++[]]]), ([0], b++[c++[]]), ([0, 0], c++[])].
  test(traverse_tree4) :-
    Tree = a++[b++[], c++[]],
    findall((Path, Node), traverse_tree(Tree, Path, Node), Results),
    Results = [([], a++[b++[], c++[]]), ([0], b++[]), ([1], c++[])].

  test(find_node1) :-
    Tree = a++[],
    findall((Path, Node), traverse_tree(Tree, Path, Node), Results),
    Path = [],
    Results = [([], a++[])].
  test(find_node2) :-
    Tree = a++[b++[]],
    Path = [],
    findall((Path, Node), traverse_tree(Tree, Path, Node), Results),
    Results = [([], a++[b++[]])].    % no b++[] because of Path = []
  test(find_node3) :-
    Tree = a++[b++[a++[]]],
    Node = a++_,
    findall((Path, Node), traverse_tree(Tree, Path, Node), Results),
    Results = [([], a++[b++[a++[]]]), ([0, 0], a++[])].    % no b++[a++[]] because of Node = a++_

:- end_tests(tree).

:- run_tests.
:- halt.
