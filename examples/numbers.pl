:- consult('../lib/vimscript.pl').
:- use_module('../lib/vimscript.pl').

main :- eval(file([
    echo([number("42") @ [1,6]]) @ [1,1],
    echo([number("1") @ [2,6]]) @ [2,1],
    echo([number("1.0") @ [3,6]]) @ [3,1],
    echo([number("1.23") @ [4,6]]) @ [4,1],
    echo([number("0xdeadbeef") @ [5,6]]) @ [5,1],
    echo([number("033") @ [6,6]]) @ [6,1],
    echo([number("1.2e-3") @ [7,6]]) @ [7,1],
    echo([number("4.5E+67") @ [8,6]]) @ [8,1],
    echo([number("4.5e8") @ [9,6]]) @ [9,1],
    comment(" vim-vimlparser doesn't support binary number literal yet...") @ [10,1],
    comment(" echo 0b1011") @ [11,1],
    comment(" echo 0b0") @ [12,1]
    ]) @ [1,1], _, _), halt.

:- main; halt(1).

