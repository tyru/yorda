:- module(tree, [
  op(500, xfy, ++),
  op(600, xfy, ::),
  new_tree/2,
  split_tree/4,
  append_tree/3,
  prepend_tree/3,
  append_tree/4,
  prepend_tree/4,
  replace_tree/4,
  get_node/3,
  get_item/3,
  find_tree/4
]).

:- op(500, xfy, ++).
:- op(600, xfy, ::).

% a++[]
% a++[b++[]]
% a++[b++[c++[]]]
% a++[b++[c++[]], d++[], e++[]]
% a++[f++[g++[]], c++[], b++[d++[], e++[]]]
new_tree(Elem++Xs, Elem++Xs) :- !.
new_tree(Elem, Elem++[]) :- not(Elem = _++_), !.

% split_tree(Tree, Path, Node, Expr :: Part).
% e.g. split_tree(a++[b++[]], [0], b++[], a++[Part] :: Part).
split_tree(Node, [], Node, Part :: Part) :- !.
split_tree(Item++[X|Xs], [0|Ns], Node, Item++[Expr|Xs] :: Part) :-
  split_tree(X, Ns, Node, Expr :: Part),
  !.
split_tree(Item++[X|Xs], [N|Ns], Node, Item++[X|Exprs] :: Part) :-
  N > 0, M is N - 1,
  split_tree(Item++Xs, [M|Ns], Node, Item++Exprs :: Part),
  !.

append_tree(Tree, Elem, NewTree) :- append_tree(Tree, [], Elem, NewTree).
prepend_tree(Tree, Elem, NewTree) :- prepend_tree(Tree, [], Elem, NewTree).
get_item(Tree, Path, Item) :- get_node(Tree, Path, Item++_).

get_node(Tree, Path, Node) :-
  split_tree(Tree, Path, Node, _ :: _).
append_tree(Tree, Path, Elem, NewTree) :-
  new_tree(Elem, ElemNode),
  split_tree(Tree, Path, Item++Children, NewTree :: Item++Added),
  append(Children, [ElemNode], Added).
prepend_tree(Tree, Path, Elem, NewTree) :-
  new_tree(Elem, ElemNode),
  split_tree(Tree, Path, Item++Children, NewTree :: Item++[ElemNode|Children]).
replace_tree(Tree, Path, Elem, NewTree) :-
  new_tree(Elem, ElemNode),
  split_tree(Tree, Path, _, NewTree :: ElemNode).

find_tree(Tree, Pred, Path, Node) :-
  find_tree([], Tree, Pred, Path, Node),
  !.
find_tree(Cur, Item++Children, Pred, Path, Node) :-
  call(Pred, Item++Children) ->
    (Node = Item++Children, Path = Cur) ;
    find_children([0|Cur], Children, Pred, Path, Node).
find_children(Cur, [X|Xs], Pred, Path, Node) :-
  find_tree(Cur, X, Pred, Path, Node);
  is_leaf(X) ->
    ([N|Ns] = Cur, M is N + 1,
      find_children([M|Ns], Xs, Pred, Path, Node)) ;
    (find_children([0|Cur], X, Pred, Path, Node);
      find_children([1|Cur], Xs, Pred, Path, Node)).
is_leaf(_++[]).
