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
  traverse_tree/3
]).

:- op(500, xfy, ++).
:- op(600, xfy, ::).

%
% Here are valid tree of this tree library.
%
%		a++[]
%		a++[b++[]]
%		a++[b++[c++[]]]
%		a++[b++[c++[]], d++[], e++[]]
%		a++[f++[g++[]], c++[], b++[d++[], e++[]]]
%
% Node is the form of `Item++Children`.
% Leaf node is `Children = []`.
%

% new_tree(+Elem, -Tree)
new_tree(Elem++Xs, Elem++Xs) :- !.
new_tree(Elem, Elem++[]) :- not(Elem = _++_), !.

% split_tree(+Tree, +Path, -Node, -Expr :: -Part).
% e.g. split_tree(a++[], [], a++[], a++[] :: a++[]).
% e.g. split_tree(a++[b++[]], [0], b++[], a++[Part] :: Part).
split_tree(Node, [], Node, Part :: Part) :- !.
split_tree(Item++[X|Xs], [0|Ns], Node, Item++[Expr|Xs] :: Part) :-
  split_tree(X, Ns, Node, Expr :: Part),
  !.
split_tree(Item++[X|Xs], [N|Ns], Node, Item++[X|Exprs] :: Part) :-
  N > 0, M is N - 1,
  split_tree(Item++Xs, [M|Ns], Node, Item++Exprs :: Part),
  !.

% append_tree(+Tree, +Elem, -NewTree)
append_tree(Tree, Elem, NewTree) :- append_tree(Tree, [], Elem, NewTree).

% prepend_tree(+Tree, +Elem, -NewTree)
prepend_tree(Tree, Elem, NewTree) :- prepend_tree(Tree, [], Elem, NewTree).

% get_item(+Tree, +Path, -Item)
get_item(Tree, Path, Item) :- get_node(Tree, Path, Item++_).

% get_node(+Tree, +Path, -Node)
get_node(Tree, Path, Node) :-
  split_tree(Tree, Path, Node, _ :: _).

% append_tree(+Tree, +Path, +Elem, -NewTree)
append_tree(Tree, Path, Elem, NewTree) :-
  new_tree(Elem, ElemNode),
  split_tree(Tree, Path, Item++Children, NewTree :: Item++Added),
  append(Children, [ElemNode], Added).

% prepend_tree(+Tree, +Path, +Elem, -NewTree)
prepend_tree(Tree, Path, Elem, NewTree) :-
  new_tree(Elem, ElemNode),
  split_tree(Tree, Path, Item++Children, NewTree :: Item++[ElemNode|Children]).

% replace_tree(+Tree, +Path, +Elem, -NewTree)
replace_tree(Tree, Path, Elem, NewTree) :-
  new_tree(Elem, ElemNode),
  split_tree(Tree, Path, _, NewTree :: ElemNode).

% traverse_tree(+Tree, -RetPath, -RetNode)
traverse_tree(Tree, Path, Node) :-
  traverse_tree([], Tree, Path, Node).
% traverse_tree(+Path, +Node, -RetPath, -RetNode)
traverse_tree(Path, Node, Path, Node).
traverse_tree(Path, _++Children, NextPath, NextNode) :-
  zip_with_index(Children, (Index, Node)),
  traverse_tree([Index|Path], Node, NextPath, NextNode).

% zip_with_index(+List, (-Index, -Elem))
zip_with_index(L, X) :-
  zip_with_index(0, L, X).
% zip_with_index(+Index, +[Elem|Xs], (-Index, -Elem))
zip_with_index(I, [X | _], (I, X)).
zip_with_index(I, [_ | Xs], Next) :-
  J is I + 1,
  zip_with_index(J, Xs, Next).
