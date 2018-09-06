:- module(tree, [
  new_tree/2,
  split_tree/4,
  append_tree/3,
  prepend_tree/3,
  append_tree/4,
  prepend_tree/4,
  get_node/3,
  get_item/3
]).

:- op(500, xfy, ->).
:- op(600, xfy, ::).

% a->[]
% a->[b->[]]
% a->[b->[c->[]]]
% a->[b->[c->[]], d->[], e->[]]
% a->[f->[g->[]], c->[], b->[d->[], e->[]]]
new_tree(Elem->Xs, Elem->Xs) :- !.
new_tree(Elem, Elem->[]) :- not(Elem = _->_), !.

% split_tree(Tree, Path, Node, Expr :: Part).
% e.g. split_tree(a->[b->[]], [0], b->[], a->[Part] :: Part).
split_tree(Node, [], Node, Part :: Part) :- !.
split_tree(Item->[X|Xs], [0|Ns], Node, Item->[Expr|Xs] :: Part) :-
  split_tree(X, Ns, Node, Expr :: Part),
  !.
split_tree(Item->[X|Xs], [N|Ns], Node, Item->[X|Exprs] :: Part) :-
  N > 0, M is N - 1,
  split_tree(Item->Xs, [M|Ns], Node, Item->Exprs :: Part),
  !.

append_tree(Tree, Elem, NewTree) :- append_tree(Tree, [], Elem, NewTree).
prepend_tree(Tree, Elem, NewTree) :- prepend_tree(Tree, [], Elem, NewTree).
get_item(Tree, Path, Item) :- get_node(Tree, Path, Item->_).

get_node(Tree, Path, Node) :-
  split_tree(Tree, Path, Node, _ :: _).
append_tree(Tree, Path, Elem, NewTree) :-
  new_tree(Elem, ElemNode),
  split_tree(Tree, Path, Item->Children, NewTree :: Item->Added),
  append(Children, [ElemNode], Added).
prepend_tree(Tree, Path, Elem, NewTree) :-
  new_tree(Elem, ElemNode),
  split_tree(Tree, Path, Item->Children, NewTree :: Item->[ElemNode|Children]).
replace_tree(Tree, Path, Elem, NewTree) :-
  new_tree(Elem, ElemNode),
  split_tree(Tree, Path, _, NewTree :: ElemNode).


:- begin_tests(tree).
  test(new_tree1) :-
    new_tree(top, top->[]).
  test(new_tree2) :-
    new_tree(node->[], node->[]).

  test(split_tree1, Part = a->[b->[], c->[]], Whole = Part) :-
    append_tree(a->[b->[], c->[]], [], Part, Whole :: xxx).
  test(split_tree2, Part = b->[], Whole = a->[xxx, c->[]]) :-
    append_tree(a->[b->[], c->[]], [0], Part, Whole :: xxx).
  test(split_tree3, Part = c->[], Whole = a->[b->[], xxx]) :-
    append_tree(a->[b->[], c->[]], [1], Part, Whole :: xxx).
  test(split_tree4) :-
    not(append_tree(a->[], [0], _, _)).
  test(split_tree5) :-
    not(append_tree(a->[], [1], _, _)).

  test(append_tree1, Ans = a->[b->[]]) :-
    append_tree(a->[], [], b, Ans).
  test(append_tree2, Ans = a->[b->[]]) :-
    append_tree(a->[], [], b->[], Ans).
  test(append_tree3, Ans = a->[b->[c->[]]]) :-
    append_tree(a->[], [], b->[c->[]], Ans).
  test(append_tree4, Ans = a->[b->[], c->[]]) :-
    append_tree(a->[b->[]], [], c->[], Ans).
  test(append_tree5, Ans = a->[b->[], c->[], d->[]]) :-
    append_tree(a->[b->[], c->[]], [], d->[], Ans).
  test(append_tree6, Ans = a->[b->[c->[]]]) :-
    append_tree(a->[b->[]], [0], c->[], Ans).
  test(append_tree7, Ans = a->[b->[], c->[d->[]]]) :-
    append_tree(a->[b->[], c->[]], [1], d->[], Ans).
  test(append_tree8, Ans = a->[b->[], c->[d->[e->[]]]]) :-
    append_tree(a->[b->[], c->[d->[]]], [1, 0], e->[], Ans).
  test(append_tree9, Ans = a->[b->[d->[], e->[f->[]]], c->[]]) :-
    append_tree(a->[b->[d->[], e->[]], c->[]], [0, 1], f->[], Ans).
  test(append_tree10, Ans = a->[b->[d->[e->[]]], c->[]]) :-
    append_tree(a->[b->[d->[]], c->[]], [0, 0], e->[], Ans).
  test(append_tree11, Ans = a->[b->[], c->[d->[], e->[f->[]]]]) :-
    append_tree(a->[b->[], c->[d->[], e->[]]], [1, 1], f->[], Ans).

  test(prepend_tree1, Ans = a->[b->[]]) :-
    prepend_tree(a->[], [], b, Ans).
  test(prepend_tree2, Ans = a->[b->[]]) :-
    prepend_tree(a->[], [], b->[], Ans).
  test(prepend_tree3, Ans = a->[b->[c->[]]]) :-
    prepend_tree(a->[], [], b->[c->[]], Ans).
  test(prepend_tree4, Ans = a->[c->[], b->[]]) :-
    prepend_tree(a->[b->[]], [], c->[], Ans).
  test(prepend_tree5, Ans = a->[d->[], b->[], c->[]]) :-
    prepend_tree(a->[b->[], c->[]], [], d->[], Ans).
  test(prepend_tree6, Ans = a->[b->[c->[]]]) :-
    prepend_tree(a->[b->[]], [0], c->[], Ans).
  test(prepend_tree7, Ans = a->[b->[], c->[d->[]]]) :-
    prepend_tree(a->[b->[], c->[]], [1], d->[], Ans).
  test(prepend_tree8, Ans = a->[b->[], c->[d->[e->[]]]]) :-
    prepend_tree(a->[b->[], c->[d->[]]], [1, 0], e->[], Ans).
  test(prepend_tree9, Ans = a->[b->[d->[], e->[f->[]]], c->[]]) :-
    prepend_tree(a->[b->[d->[], e->[]], c->[]], [0, 1], f->[], Ans).
  test(prepend_tree10, Ans = a->[b->[d->[e->[]]], c->[]]) :-
    prepend_tree(a->[b->[d->[]], c->[]], [0, 0], e->[], Ans).
  test(prepend_tree11, Ans = a->[b->[], c->[d->[], e->[f->[]]]]) :-
    prepend_tree(a->[b->[], c->[d->[], e->[]]], [1, 1], f->[], Ans).

  test(replace_tree1, Ans = b->[]) :-
    replace_tree(a->[], [], b, Ans).
  test(replace_tree2, Ans = b->[]) :-
    replace_tree(a->[], [], b->[], Ans).
  test(replace_tree3, Ans = a->[c->[]]) :-
    replace_tree(a->[b->[]], [0], c->[], Ans).
  test(replace_tree4, Ans = a->[b->[], d->[]]) :-
    replace_tree(a->[b->[], c->[]], [1], d->[], Ans).
  test(replace_tree5, Ans = a->[b->[], c->[e->[]]]) :-
    replace_tree(a->[b->[], c->[d->[]]], [1, 0], e->[], Ans).
  test(replace_tree6, Ans = a->[b->[d->[], f->[]], c->[]]) :-
    replace_tree(a->[b->[d->[], e->[]], c->[]], [0, 1], f->[], Ans).
  test(replace_tree7, Ans = a->[b->[e->[]], c->[]]) :-
    replace_tree(a->[b->[d->[]], c->[]], [0, 0], e->[], Ans).
  test(replace_tree8, Ans = a->[b->[], c->[d->[], f->[]]]) :-
    replace_tree(a->[b->[], c->[d->[], e->[]]], [1, 1], f->[], Ans).

  test(get_node1) :-
    get_node(a->[], [], a->[]).
  test(get_node2) :-
    get_node(a->[b->[c->[]]], [0], b->[c->[]]).
  test(get_node3) :-
    get_node(a->[b->[c->[]]], [0,0], c->[]).
  test(get_node4) :-
    get_node(a->[b->[c->[]], d->[e->[]]], [1], d->[e->[]]).
  test(get_node5) :-
    get_node(a->[b->[c->[]], d->[e->[]]], [1,0], e->[]).

  test(get_item1) :-
    get_item(a->[], [], a).
  test(get_item2) :-
    get_item(a->[b->[c->[]]], [0], b).
  test(get_item3) :-
    get_item(a->[b->[c->[]]], [0,0], c).
  test(get_item4) :-
    get_item(a->[b->[c->[]], d->[e->[]]], [1], d).
  test(get_item5) :-
    get_item(a->[b->[c->[]], d->[e->[]]], [1,0], e).
:- end_tests(tree).

:- run_tests.
:- halt.
