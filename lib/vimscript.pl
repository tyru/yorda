:- module(vimscript, [
  op(501, xfy, @),
  (@)/2,
  op(502, xfy, =>),
  (=>)/2,
  op(503, xfy, ::),
  (::)/2,
  eval/3
]).

% ===================== Operators =====================

:- multifile (@)/2.
:- multifile (=>)/2.
:- multifile (::)/2.
:- op(501, xfy, @).
:- op(502, xfy, =>).
:- op(503, xfy, ::).

% ===================== Utilities =====================

empty([]).

succ(N, M) :- nonvar(N), !, M is N + 1.
succ(N, M) :- nonvar(M), !, N is M - 1.

pred(N, M) :- nonvar(N), !, M is N - 1.
pred(N, M) :- nonvar(M), !, N is M + 1.

% update_assoc_list(+Elem, ?List, +Updated, ?UpdatedList)
%
% NOTE: Elem must be matched to one or zero elements in List.
% update_assoc_list(N, [1,2,3,1], M, L).
% N = 1, L = [M,2,3,M]
%
update_assoc_list(Elem, List, Updated, UpdatedList) :-
  maplist(update(Elem, Updated), List, UpdatedList).
update(Elem, Updated, Elem, Updated).
update(X, _, Y, Y) :- \+ X = Y.

% split_list(+N, ?List, ?Taken, ?Dropped)
split_list(0, Dropped, [], Dropped) :- !.
split_list(N, [X | List], [X | Taken], Dropped) :-
  N > 0, M is N - 1, split_list(M, List, Taken, Dropped), !.

error(Format, Args, tError(Msg)) :-
  format(atom(Msg), Format, Args), !.

pos_string([Line, Col], S) :- format(atom(S), '~d,~d: ', [Line, Col]).
pos_string(Pos, '') :- \+ Pos = [_, _].

% ===================== Primitive types =====================

prim(tAny).
prim(tVoid).
% v:true = tBool(true), v:false = tBool(false)
prim(tBool(_)).
% v:null = tNone(null), v:none = tNone(none)
prim(tNone(_)).
% 42 = tInt(42)
prim(tInt(_)).
% 12.34 = tFloat(12.34)
prim(tFloat(_)).
% "hello" = tString("hello")
prim(tString(_)).
% [42] = tList([tInt(42) @ P])
prim(tList(_)).
% {'foo': 1, 'bar': 2} = tDict([
%   tString("foo") @ P1 : tInt(1) @ P2,
%   tString("bar") @ P3 : tInt(2) @ P4
% ])
prim(tDict(_)).
% [42, 12.34, "string"] = tTuple([tInt(42) @ P1, tFloat(12.34) @ P2, tString("string") @ P3])
prim(tTuple(_)).
prim(tError(_)).    % tError(Msg)

to_bool(tBool(V), tBool(V)).
to_bool(tNone(_), tBool(false)).
to_bool(tInt(_), tBool(_)).
to_bool(tString(_), tBool(_)).
to_bool(tAny, tBool(_)).

to_string(tString(V), tString(V)).
to_string(tBool(_), tString(_)).
to_string(tNone(_), tString(_)).
to_string(tInt(_), tString(_)).
to_string(tAny, tString(_)).

to_int(tInt(V), tInt(V)).
to_int(tString(_), tInt(_)).
to_int(tBool(_), tInt(_)).
to_int(tNone(_), tInt(_)).
to_int(tAny, tInt(_)).

get_prop(tAny, _, tAny).
get_prop(_, tAny, tAny).
get_prop(tDict(_), Right, tAny) :- to_string(Right, _).
get_prop(tString(_), Right, tAny) :- to_int(Right, _).

% ===================== Built-in functions =====================

% vimfunc(Env, Name, Type)

% abs(tInt(V)) = tInt(V)
% abs(tFloat(V)) = tFloat(V)
vimfunc(_, "abs", [tInt(V) @ _] :: tInt(V) @ _).
vimfunc(_, "abs", [tFloat(V) @ _] :: tFloat(V) @ _).
% acos(tFloat) = tFloat
vimfunc(_, "acos", [tFloat(_) @ _] :: tFloat(_) @ _).
% add(L, E) = [E|L]
vimfunc(_, "add", [tList(L) @ _, E] :: tList([E|L]) @ _).
% and(tInt, tInt) = tInt
vimfunc(_, "and", [tInt(_) @ _, tInt(_) @ _] :: tInt(_) @ _).
% append(tInt | tString, tList | tString) = tInt(0)
vimfunc(_, "append", [tInt(_) @ _, tList(_) @ _] :: tInt(0) @ _).
vimfunc(Env, "append", [tInt(A) @ _, tString(B) @ SPos] :: R @ _) :-
  vimfunc(Env, "append", [tInt(A) @ _, tList([tString(B) @ SPos]) @ _] :: R @ _).
vimfunc(_, "append", [tString(_) @ _, tList(_) @ _] :: tInt(0) @ _).
vimfunc(Env, "append", [tString(A) @ _, tString(B) @ SPos] :: R @ _) :-
  vimfunc(Env, "append", [tString(A) @ _, tList([tString(B) @ SPos]) @ _] :: R @ _).
% appendbufline(tInt | tString, tInt | tString, tList | tString) = tInt(0)
vimfunc(_, "appendbufline", [tInt(_) @ _, tInt(_) @ _, tList(_) @ _] :: tInt(0) @ _).
vimfunc(Env, "appendbufline", [tInt(A) @ _, tInt(B) @ _, tString(C) @ SPos] :: R @ _) :-
  vimfunc(Env, "appendbufline", [tInt(A) @ _, tInt(B) @ _, tList([tString(C) @ SPos]) @ _] :: R @ _).
vimfunc(_, "appendbufline", [tInt(_) @ _, tString(_) @ _, tList(_) @ _] :: tInt(0) @ _).
vimfunc(Env, "appendbufline", [tInt(A) @ _, tString(B) @ _, tString(C) @ SPos] :: R @ _) :-
  vimfunc(Env, "appendbufline", [tInt(A) @ _, tString(B) @ _, tList([tString(C) @ SPos]) @ _] :: R @ _).
vimfunc(_, "appendbufline", [tString(_) @ _, tInt(_) @ _, tList(_) @ _] :: tInt(0) @ _).
vimfunc(Env, "appendbufline", [tString(A) @ _, tInt(B) @ _, tString(C) @ SPos] :: R @ _) :-
  vimfunc(Env, "appendbufline", [tString(A) @ _, tInt(B) @ _, tList([tString(C) @ SPos]) @ _] :: R @ _).
vimfunc(_, "appendbufline", [tString(_) @ _, tString(_) @ _, tList(_) @ _] :: tInt(0) @ _).
vimfunc(Env, "appendbufline", [tString(A) @ _, tString(B) @ _, tString(C) @ SPos] :: R @ _) :-
  vimfunc(Env, "appendbufline", [tString(A) @ _, tString(B) @ _, tList([tString(C) @ SPos]) @ _] :: R @ _).
% call(X, Args, Self)
% add Self to Env, and call call(X, Args) recursively.
vimfunc(Env, "call", [X, tList(Args) @ _, Self] :: R @ _) :-
  NextEnv = [ident("l", "self") @ _ => Self | Env],
  vimfunc(NextEnv, "call", [X, tList(Args) @ _] :: R @ _), !.
% call(Fun, Args)
vimfunc(_, "call", [Fun, tList(Args) @ _] :: R @ _) :- R = call(Fun, Args), !.
% Name must not be a variable, and must be a vimfunc.
vimfunc(_, "function", [tString(Name) @ _] :: R @ _) :-
  var(Name), R = _ :: _, !.
vimfunc(Env, "function", [tString(Name) @ _] :: FunT @ _) :-
  nonvar(Name), vimfunc(Env, Name, FunT @ _), !.
% tr(tString, tString, tString) = tString
vimfunc(_, "tr", [tString(_) @ _, tString(_) @ _, tString(_) @ _] :: tString(_) @ _) :- !.
% has(tString) = tInt(1) | tInt(0)
vimfunc(_, "has", [tString(_) @ _] :: tInt(1) @ _).
vimfunc(_, "has", [tString(_) @ _] :: tInt(0) @ _).

% TODO
% vimfunc(_, "argc", _).
% vimfunc(_, "argidx", _).
% vimfunc(_, "arglistid", _).
% vimfunc(_, "argv", _).
% vimfunc(_, "assert_beeps", _).
% vimfunc(_, "assert_equal", _).
% vimfunc(_, "assert_equalfile", _).
% vimfunc(_, "assert_exception", _).
% vimfunc(_, "assert_fails", _).
% vimfunc(_, "assert_false", _).
% vimfunc(_, "assert_inrange", _).
% vimfunc(_, "assert_match", _).
% vimfunc(_, "assert_notequal", _).
% vimfunc(_, "assert_notmatch", _).
% vimfunc(_, "assert_report", _).
% vimfunc(_, "assert_true", _).
% vimfunc(_, "asin", _).
% vimfunc(_, "atan", _).
% vimfunc(_, "atan2", _).
% vimfunc(_, "balloon_show", _).
% vimfunc(_, "balloon_split", _).
% vimfunc(_, "browse", _).
% vimfunc(_, "browsedir", _).
% vimfunc(_, "bufexists", _).
% vimfunc(_, "buflisted", _).
% vimfunc(_, "bufloaded", _).
% vimfunc(_, "bufname", _).
% vimfunc(_, "bufnr", _).
% vimfunc(_, "bufwinid", _).
% vimfunc(_, "bufwinnr", _).
% vimfunc(_, "byte2line", _).
% vimfunc(_, "byteidx", _).
% vimfunc(_, "byteidxcomp", _).
% vimfunc(_, "ceil", _).
% vimfunc(_, "ch_canread", _).
% vimfunc(_, "ch_close", _).
% vimfunc(_, "ch_close_in", _).
% vimfunc(_, "ch_evalexpr", _).
% vimfunc(_, "ch_evalraw", _).
% vimfunc(_, "ch_getbufnr", _).
% vimfunc(_, "ch_getjob", _).
% vimfunc(_, "ch_info", _).
% vimfunc(_, "ch_log", _).
% vimfunc(_, "ch_logfile", _).
% vimfunc(_, "ch_open", _).
% vimfunc(_, "ch_read", _).
% vimfunc(_, "ch_readraw", _).
% vimfunc(_, "ch_sendexpr", _).
% vimfunc(_, "ch_sendraw", _).
% vimfunc(_, "ch_setoptions", _).
% vimfunc(_, "ch_status", _).
% vimfunc(_, "changenr", _).
% vimfunc(_, "char2nr", _).
% vimfunc(_, "cindent", _).
% vimfunc(_, "clearmatches", _).
% vimfunc(_, "col", _).
% vimfunc(_, "complete", _).
% vimfunc(_, "complete_add", _).
% vimfunc(_, "complete_check", _).
% vimfunc(_, "confirm", _).
% vimfunc(_, "copy", _).
% vimfunc(_, "cos", _).
% vimfunc(_, "cosh", _).
% vimfunc(_, "count", _).
% vimfunc(_, "cscope_connection", _).
% vimfunc(_, "cursor", _).
% vimfunc(_, "debugbreak", _).
% vimfunc(_, "deepcopy", _).
% vimfunc(_, "delete", _).
% vimfunc(_, "deletebufline", _).
% vimfunc(_, "did_filetype", _).
% vimfunc(_, "diff_filler", _).
% vimfunc(_, "diff_hlID", _).
% vimfunc(_, "empty", _).
% vimfunc(_, "escape", _).
% vimfunc(_, "eval", _).
% vimfunc(_, "eventhandler", _).
% vimfunc(_, "executable", _).
% vimfunc(_, "execute", _).
% vimfunc(_, "exepath", _).
% vimfunc(_, "exists", _).
% vimfunc(_, "extend", _).
% vimfunc(_, "exp", _).
% vimfunc(_, "expand", _).
% vimfunc(_, "feedkeys", _).
% vimfunc(_, "filereadable", _).
% vimfunc(_, "filewritable", _).
% vimfunc(_, "filter", _).
% vimfunc(_, "finddir", _).
% vimfunc(_, "findfile", _).
% vimfunc(_, "float2nr", _).
% vimfunc(_, "floor", _).
% vimfunc(_, "fmod", _).
% vimfunc(_, "fnameescape", _).
% vimfunc(_, "fnamemodify", _).
% vimfunc(_, "foldclosed", _).
% vimfunc(_, "foldclosedend", _).
% vimfunc(_, "foldlevel", _).
% vimfunc(_, "foldtext", _).
% vimfunc(_, "foldtextresult", _).
% vimfunc(_, "foreground", _).
% vimfunc(_, "funcref", _).
% vimfunc(_, "garbagecollect", _).
% vimfunc(_, "get", _).
% vimfunc(_, "getbufinfo", _).
% vimfunc(_, "getbufline", _).
% vimfunc(_, "getbufvar", _).
% vimfunc(_, "getchangelist", _).
% vimfunc(_, "getchar", _).
% vimfunc(_, "getcharmod", _).
% vimfunc(_, "getcharsearch", _).
% vimfunc(_, "getcmdline", _).
% vimfunc(_, "getcmdpos", _).
% vimfunc(_, "getcmdtype", _).
% vimfunc(_, "getcmdwintype", _).
% vimfunc(_, "getcompletion", _).
% vimfunc(_, "getcurpos", _).
% vimfunc(_, "getcwd", _).
% vimfunc(_, "getfontname", _).
% vimfunc(_, "getfperm", _).
% vimfunc(_, "getfsize", _).
% vimfunc(_, "getftime", _).
% vimfunc(_, "getftype", _).
% vimfunc(_, "getjumplist", _).
% vimfunc(_, "getline", _).
% vimfunc(_, "getloclist", _).
% vimfunc(_, "getmatches", _).
% vimfunc(_, "getpid", _).
% vimfunc(_, "getpos", _).
% vimfunc(_, "getqflist", _).
% vimfunc(_, "getreg", _).
% vimfunc(_, "getregtype", _).
% vimfunc(_, "gettabinfo", _).
% vimfunc(_, "gettabvar", _).
% vimfunc(_, "gettabwinvar", _).
% vimfunc(_, "getwininfo", _).
% vimfunc(_, "getwinpos", _).
% vimfunc(_, "getwinposx", _).
% vimfunc(_, "getwinposy", _).
% vimfunc(_, "getwinvar", _).
% vimfunc(_, "glob", _).
% vimfunc(_, "glob2regpat", _).
% vimfunc(_, "globpath", _).
% vimfunc(_, "has_key", _).
% vimfunc(_, "haslocaldir", _).
% vimfunc(_, "hasmapto", _).
% vimfunc(_, "histadd", _).
% vimfunc(_, "histdel", _).
% vimfunc(_, "histget", _).
% vimfunc(_, "histnr", _).
% vimfunc(_, "hlexists", _).
% vimfunc(_, "hlID", _).
% vimfunc(_, "hostname", _).
% vimfunc(_, "iconv", _).
% vimfunc(_, "indent", _).
% vimfunc(_, "index", _).
% vimfunc(_, "input", _).
% vimfunc(_, "inputdialog", _).
% vimfunc(_, "inputlist", _).
% vimfunc(_, "inputrestore", _).
% vimfunc(_, "inputsave", _).
% vimfunc(_, "inputsecret", _).
% vimfunc(_, "insert", _).
% vimfunc(_, "invert", _).
% vimfunc(_, "isdirectory", _).
% vimfunc(_, "islocked", _).
% vimfunc(_, "isnan", _).
% vimfunc(_, "items", _).
% vimfunc(_, "job_getchannel", _).
% vimfunc(_, "job_info", _).
% vimfunc(_, "job_setoptions", _).
% vimfunc(_, "job_start", _).
% vimfunc(_, "job_status", _).
% vimfunc(_, "job_stop", _).
% vimfunc(_, "join", _).
% vimfunc(_, "js_decode", _).
% vimfunc(_, "js_encode", _).
% vimfunc(_, "json_decode", _).
% vimfunc(_, "json_encode", _).
% vimfunc(_, "keys", _).
% vimfunc(_, "len", _).
% vimfunc(_, "libcall", _).
% vimfunc(_, "libcallnr", _).
% vimfunc(_, "line", _).
% vimfunc(_, "line2byte", _).
% vimfunc(_, "lispindent", _).
% vimfunc(_, "localtime", _).
% vimfunc(_, "log", _).
% vimfunc(_, "log10", _).
% vimfunc(_, "luaeval", _).
% vimfunc(_, "map", _).
% vimfunc(_, "maparg", _).
% vimfunc(_, "mapcheck", _).
% vimfunc(_, "match", _).
% vimfunc(_, "matchadd", _).
% vimfunc(_, "matchaddpos", _).
% vimfunc(_, "matcharg", _).
% vimfunc(_, "matchdelete", _).
% vimfunc(_, "matchend", _).
% vimfunc(_, "matchlist", _).
% vimfunc(_, "matchstr", _).
% vimfunc(_, "matchstrpos", _).
% vimfunc(_, "max", _).
% vimfunc(_, "min", _).
% vimfunc(_, "mkdir", _).
% vimfunc(_, "mode", _).
% vimfunc(_, "mzeval", _).
% vimfunc(_, "nextnonblank", _).
% vimfunc(_, "nr2char", _).
% vimfunc(_, "or", _).
% vimfunc(_, "pathshorten", _).
% vimfunc(_, "perleval", _).
% vimfunc(_, "pow", _).
% vimfunc(_, "prevnonblank", _).
% vimfunc(_, "printf", _).
% vimfunc(_, "prompt_addtext", _).
% vimfunc(_, "prompt_setcallback", _).
% vimfunc(_, "prompt_setinterrupt", _).
% vimfunc(_, "prompt_setprompt", _).
% vimfunc(_, "pumvisible", _).
% vimfunc(_, "pyeval", _).
% vimfunc(_, "py3eval", _).
% vimfunc(_, "pyxeval", _).
% vimfunc(_, "range", _).
% vimfunc(_, "readfile", _).
% vimfunc(_, "reg_executing", _).
% vimfunc(_, "reg_recording", _).
% vimfunc(_, "reltime", _).
% vimfunc(_, "reltimefloat", _).
% vimfunc(_, "reltimestr", _).
% vimfunc(_, "remote_expr", _).
% vimfunc(_, "remote_foreground", _).
% vimfunc(_, "remote_peek", _).
% vimfunc(_, "remote_read", _).
% vimfunc(_, "remote_send", _).
% vimfunc(_, "remote_startserver", _).
% vimfunc(_, "remove", _).
% vimfunc(_, "rename", _).
% vimfunc(_, "repeat", _).
% vimfunc(_, "resolve", _).
% vimfunc(_, "reverse", _).
% vimfunc(_, "round", _).
% vimfunc(_, "screenattr", _).
% vimfunc(_, "screenchar", _).
% vimfunc(_, "screencol", _).
% vimfunc(_, "screenrow", _).
% vimfunc(_, "search", _).
% vimfunc(_, "searchdecl", _).
% vimfunc(_, "searchpair", _).
% vimfunc(_, "searchpairpos", _).
% vimfunc(_, "searchpos", _).
% vimfunc(_, "server2client", _).
% vimfunc(_, "serverlist", _).
% vimfunc(_, "setbufline", _).
% vimfunc(_, "setbufvar", _).
% vimfunc(_, "setcharsearch", _).
% vimfunc(_, "setcmdpos", _).
% vimfunc(_, "setfperm", _).
% vimfunc(_, "setline", _).
% vimfunc(_, "setloclist", _).
% vimfunc(_, "setmatches", _).
% vimfunc(_, "setpos", _).
% vimfunc(_, "setqflist", _).
% vimfunc(_, "setreg", _).
% vimfunc(_, "settabvar", _).
% vimfunc(_, "settabwinvar", _).
% vimfunc(_, "setwinvar", _).
% vimfunc(_, "sha256", _).
% vimfunc(_, "shellescape", _).
% vimfunc(_, "shiftwidth", _).
% vimfunc(_, "simplify", _).
% vimfunc(_, "sin", _).
% vimfunc(_, "sinh", _).
% vimfunc(_, "sort", _).
% vimfunc(_, "soundfold", _).
% vimfunc(_, "spellbadword", _).
% vimfunc(_, "spellsuggest", _).
% vimfunc(_, "split", _).
% vimfunc(_, "sqrt", _).
% vimfunc(_, "str2float", _).
% vimfunc(_, "str2nr", _).
% vimfunc(_, "strchars", _).
% vimfunc(_, "strcharpart", _).
% vimfunc(_, "strdisplaywidth", _).
% vimfunc(_, "strftime", _).
% vimfunc(_, "strgetchar", _).
% vimfunc(_, "stridx", _).
% vimfunc(_, "string", _).
% vimfunc(_, "strlen", _).
% vimfunc(_, "strpart", _).
% vimfunc(_, "strridx", _).
% vimfunc(_, "strtrans", _).
% vimfunc(_, "strwidth", _).
% vimfunc(_, "submatch", _).
% vimfunc(_, "substitute", _).
% vimfunc(_, "swapinfo", _).
% vimfunc(_, "synID", _).
% vimfunc(_, "synIDattr", _).
% vimfunc(_, "synIDtrans", _).
% vimfunc(_, "synconcealed", _).
% vimfunc(_, "synstack", _).
% vimfunc(_, "system", _).
% vimfunc(_, "systemlist", _).
% vimfunc(_, "tabpagebuflist", _).
% vimfunc(_, "tabpagenr", _).
% vimfunc(_, "tabpagewinnr", _).
% vimfunc(_, "taglist", _).
% vimfunc(_, "tagfiles", _).
% vimfunc(_, "tan", _).
% vimfunc(_, "tanh", _).
% vimfunc(_, "tempname", _).
% vimfunc(_, "term_dumpdiff", _).
% vimfunc(_, "term_dumpload", _).
% vimfunc(_, "term_dumpwrite", _).
% vimfunc(_, "term_getaltscreen", _).
% vimfunc(_, "term_getansicolors", _).
% vimfunc(_, "term_getattr", _).
% vimfunc(_, "term_getcursor", _).
% vimfunc(_, "term_getjob", _).
% vimfunc(_, "term_getline", _).
% vimfunc(_, "term_getscrolled", _).
% vimfunc(_, "term_getsize", _).
% vimfunc(_, "term_getstatus", _).
% vimfunc(_, "term_gettitle", _).
% vimfunc(_, "term_gettty", _).
% vimfunc(_, "term_list", _).
% vimfunc(_, "term_scrape", _).
% vimfunc(_, "term_sendkeys", _).
% vimfunc(_, "term_setansicolors", _).
% vimfunc(_, "term_setkill", _).
% vimfunc(_, "term_setrestore", _).
% vimfunc(_, "term_setsize", _).
% vimfunc(_, "term_start", _).
% vimfunc(_, "term_wait", _).
% vimfunc(_, "test_alloc_fail", _).
% vimfunc(_, "test_autochdir", _).
% vimfunc(_, "test_feedinput", _).
% vimfunc(_, "test_garbagecollect_now", _).
% vimfunc(_, "test_ignore_error", _).
% vimfunc(_, "test_null_channel", _).
% vimfunc(_, "test_null_dict", _).
% vimfunc(_, "test_null_job", _).
% vimfunc(_, "test_null_list", _).
% vimfunc(_, "test_null_partial", _).
% vimfunc(_, "test_null_string", _).
% vimfunc(_, "test_override", _).
% vimfunc(_, "test_settime", _).
% vimfunc(_, "timer_info", _).
% vimfunc(_, "timer_pause", _).
% vimfunc(_, "timer_start", _).
% vimfunc(_, "timer_stop", _).
% vimfunc(_, "timer_stopall", _).
% vimfunc(_, "tolower", _).
% vimfunc(_, "toupper", _).
% vimfunc(_, "trim", _).
% vimfunc(_, "trunc", _).
% vimfunc(_, "type", _).
% vimfunc(_, "undofile", _).
% vimfunc(_, "undotree", _).
% vimfunc(_, "uniq", _).
% vimfunc(_, "values", _).
% vimfunc(_, "virtcol", _).
% vimfunc(_, "visualmode", _).
% vimfunc(_, "wildmenumode", _).
% vimfunc(_, "win_findbuf", _).
% vimfunc(_, "win_getid", _).
% vimfunc(_, "win_gotoid", _).
% vimfunc(_, "win_id2tabwin", _).
% vimfunc(_, "win_id2win", _).
% vimfunc(_, "win_screenpos", _).
% vimfunc(_, "winbufnr", _).
% vimfunc(_, "wincol", _).
% vimfunc(_, "winheight", _).
% vimfunc(_, "winlayout", _).
% vimfunc(_, "winline", _).
% vimfunc(_, "winnr", _).
% vimfunc(_, "winrestcmd", _).
% vimfunc(_, "winrestview", _).
% vimfunc(_, "winsaveview", _).
% vimfunc(_, "winwidth", _).
% vimfunc(_, "wordcount", _).
% vimfunc(_, "writefile", _).
% vimfunc(_, "xor", _).

% vimoption(?Name, ?Type)
vimoption(Name, Type) :- vimoption(Name, _, Type); vimoption(_, Name, Type).

% ===================== Vim options =====================

vimoption("aleph", "al", tInt(_)).
vimoption("allowrevins", "ari", tBool(_)).
vimoption("altkeymap", "akm", tBool(_)).
vimoption("ambiwidth", "ambw", tString(_)).
vimoption("antialias", "anti", tBool(_)).
vimoption("autochdir", "acd", tBool(_)).
vimoption("arabic", "arab", tBool(_)).
vimoption("arabicshape", "arshape", tBool(_)).
vimoption("autoindent", "ai", tBool(_)).
vimoption("autoread", "ar", tBool(_)).
vimoption("autowrite", "aw", tBool(_)).
vimoption("autowriteall", "awa", tBool(_)).
vimoption("background", "bg", tString(_)).
vimoption("backspace", "bs", tString(_)).
vimoption("backup", "bk", tBool(_)).
vimoption("backupcopy", "bkc", tString(_)).
vimoption("backupdir", "bdir", tString(_)).
vimoption("backupext", "bex", tString(_)).
vimoption("backupskip", "bsk", tString(_)).
vimoption("balloondelay", "bdlay", tInt(_)).
vimoption("ballooneval", "beval", tBool(_)).
vimoption("balloonevalterm", "bevalterm", tBool(_)).
vimoption("balloonexpr", "bexpr", tString(_)).
vimoption("belloff", "bo", tString(_)).
vimoption("binary", "bin", tBool(_)).
vimoption("bioskey", "biosk", tBool(_)).
vimoption("bomb", "", tBool(_)).
vimoption("breakat", "brk", tString(_)).
vimoption("breakindent", "bri", tBool(_)).
vimoption("breakindentopt", "briopt", tString(_)).
vimoption("browsedir", "bsdir", tString(_)).
vimoption("bufhidden", "bh", tString(_)).
vimoption("buflisted", "bl", tBool(_)).
vimoption("buftype", "bt", tString(_)).
vimoption("casemap", "cmp", tString(_)).
vimoption("cdpath", "cd", tString(_)).
vimoption("cedit", "", tString(_)).
vimoption("charconvert", "ccv", tString(_)).
vimoption("cindent", "cin", tBool(_)).
vimoption("cinkeys", "cink", tString(_)).
vimoption("cinoptions", "cino", tString(_)).
vimoption("cinwords", "cinw", tString(_)).
vimoption("clipboard", "cb", tString(_)).
vimoption("cmdheight", "ch", tInt(_)).
vimoption("cmdwinheight", "cwh", tInt(_)).
vimoption("colorcolumn", "cc", tString(_)).
vimoption("columns", "co", tInt(_)).
vimoption("comments", "com", tString(_)).
vimoption("commentstring", "cms", tString(_)).
vimoption("compatible", "cp", tBool(_)).
vimoption("complete", "cpt", tString(_)).
vimoption("completefunc", "cfu", tString(_)).
vimoption("completeopt", "cot", tString(_)).
vimoption("concealcursor", "cocu", tString(_)).
vimoption("conceallevel", "cole", tInt(_)).
vimoption("confirm", "cf", tBool(_)).
vimoption("conskey", "consk", tBool(_)).
vimoption("copyindent", "ci", tBool(_)).
vimoption("cpoptions", "cpo", tString(_)).
vimoption("cryptmethod", "cm", tString(_)).
vimoption("cscopepathcomp", "cspc", tInt(_)).
vimoption("cscopeprg", "csprg", tString(_)).
vimoption("cscopequickfix", "csqf", tString(_)).
vimoption("cscoperelative", "csre", tBool(_)).
vimoption("cscopetag", "cst", tBool(_)).
vimoption("cscopetagorder", "csto", tInt(_)).
vimoption("cscopeverbose", "csverb", tBool(_)).
vimoption("cursorbind", "crb", tBool(_)).
vimoption("cursorcolumn", "cuc", tBool(_)).
vimoption("cursorline", "cul", tBool(_)).
vimoption("debug", "", tString(_)).
vimoption("define", "def", tString(_)).
vimoption("delcombine", "deco", tBool(_)).
vimoption("dictionary", "dict", tString(_)).
vimoption("diff", "", tBool(_)).
vimoption("diffexpr", "dex", tString(_)).
vimoption("diffopt", "dip", tString(_)).
vimoption("digraph", "dg", tBool(_)).
vimoption("directory", "dir", tString(_)).
vimoption("display", "dy", tString(_)).
vimoption("eadirection", "ead", tString(_)).
vimoption("edcompatible", "ed", tBool(_)).
vimoption("emoji", "emo", tBool(_)).
vimoption("encoding", "enc", tString(_)).
vimoption("endofline", "eol", tBool(_)).
vimoption("equalalways", "ea", tBool(_)).
vimoption("equalprg", "ep", tString(_)).
vimoption("errorbells", "eb", tBool(_)).
vimoption("errorfile", "ef", tString(_)).
vimoption("errorformat", "efm", tString(_)).
vimoption("esckeys", "ek", tBool(_)).
vimoption("eventignore", "ei", tString(_)).
vimoption("expandtab", "et", tBool(_)).
vimoption("exrc", "ex", tBool(_)).
vimoption("fileencoding", "fenc", tString(_)).
vimoption("fileencodings", "fencs", tString(_)).
vimoption("fileformat", "ff", tString(_)).
vimoption("fileformats", "ffs", tString(_)).
vimoption("fileignorecase", "fic", tBool(_)).
vimoption("filetype", "ft", tString(_)).
vimoption("fillchars", "fcs", tString(_)).
vimoption("fixendofline", "fixeol", tBool(_)).
vimoption("fkmap", "fk", tBool(_)).
vimoption("foldclose", "fcl", tString(_)).
vimoption("foldcolumn", "fdc", tInt(_)).
vimoption("foldenable", "fen", tBool(_)).
vimoption("foldexpr", "fde", tString(_)).
vimoption("foldignore", "fdi", tString(_)).
vimoption("foldlevel", "fdl", tInt(_)).
vimoption("foldlevelstart", "fdls", tInt(_)).
vimoption("foldmarker", "fmr", tString(_)).
vimoption("foldmethod", "fdm", tString(_)).
vimoption("foldminlines", "fml", tInt(_)).
vimoption("foldnestmax", "fdn", tInt(_)).
vimoption("foldopen", "fdo", tString(_)).
vimoption("foldtext", "fdt", tString(_)).
vimoption("formatexpr", "fex", tString(_)).
vimoption("formatoptions", "fo", tString(_)).
vimoption("formatlistpat", "flp", tString(_)).
vimoption("formatprg", "fp", tString(_)).
vimoption("fsync", "fs", tBool(_)).
vimoption("gdefault", "gd", tBool(_)).
vimoption("grepformat", "gfm", tString(_)).
vimoption("grepprg", "gp", tString(_)).
vimoption("guicursor", "gcr", tString(_)).
vimoption("guifont", "gfn", tString(_)).
vimoption("guifontset", "gfs", tString(_)).
vimoption("guifontwide", "gfw", tString(_)).
vimoption("guiheadroom", "ghr", tInt(_)).
vimoption("guioptions", "go", tString(_)).
vimoption("guipty", "", tBool(_)).
vimoption("guitablabel", "gtl", tString(_)).
vimoption("guitabtooltip", "gtt", tString(_)).
vimoption("helpfile", "hf", tString(_)).
vimoption("helpheight", "hh", tInt(_)).
vimoption("helplang", "hlg", tString(_)).
vimoption("hidden", "hid", tBool(_)).
vimoption("highlight", "hl", tString(_)).
vimoption("history", "hi", tInt(_)).
vimoption("hkmap", "hk", tBool(_)).
vimoption("hkmapp", "hkp", tBool(_)).
vimoption("hlsearch", "hls", tBool(_)).
vimoption("icon", "", tBool(_)).
vimoption("iconstring", "", tString(_)).
vimoption("ignorecase", "ic", tBool(_)).
vimoption("imactivatefunc", "imaf", tString(_)).
vimoption("imactivatekey", "imak", tString(_)).
vimoption("imcmdline", "imc", tBool(_)).
vimoption("imdisable", "imd", tBool(_)).
vimoption("iminsert", "imi", tInt(_)).
vimoption("imsearch", "ims", tInt(_)).
vimoption("imstatusfunc", "imsf", tString(_)).
vimoption("imstyle", "imst", tInt(_)).
vimoption("include", "inc", tString(_)).
vimoption("includeexpr", "inex", tString(_)).
vimoption("incsearch", "is", tBool(_)).
vimoption("indentexpr", "inde", tString(_)).
vimoption("indentkeys", "indk", tString(_)).
vimoption("infercase", "inf", tBool(_)).
vimoption("insertmode", "im", tBool(_)).
vimoption("isfname", "isf", tString(_)).
vimoption("isident", "isi", tString(_)).
vimoption("iskeyword", "isk", tString(_)).
vimoption("isprint", "isp", tString(_)).
vimoption("joinspaces", "js", tBool(_)).
vimoption("key", "", tString(_)).
vimoption("keymap", "kmp", tString(_)).
vimoption("keymodel", "km", tString(_)).
vimoption("keywordprg", "kp", tString(_)).
vimoption("langmap", "lmap", tString(_)).
vimoption("langmenu", "lm", tString(_)).
vimoption("langnoremap", "lnr", tBool(_)).
vimoption("langremap", "lrm", tBool(_)).
vimoption("laststatus", "ls", tInt(_)).
vimoption("lazyredraw", "lz", tBool(_)).
vimoption("linebreak", "lbr", tBool(_)).
vimoption("lines", "", tInt(_)).
vimoption("linespace", "lsp", tInt(_)).
vimoption("lisp", "", tBool(_)).
vimoption("lispwords", "lw", tString(_)).
vimoption("list", "", tBool(_)).
vimoption("listchars", "lcs", tString(_)).
vimoption("loadplugins", "lpl", tBool(_)).
vimoption("luadll", "", tString(_)).
vimoption("macatsui", "", tBool(_)).
vimoption("magic", "", tBool(_)).
vimoption("makeef", "mef", tString(_)).
vimoption("makeencoding", "menc", tString(_)).
vimoption("makeprg", "mp", tString(_)).
vimoption("matchpairs", "mps", tString(_)).
vimoption("matchtime", "mat", tInt(_)).
vimoption("maxcombine", "mco", tInt(_)).
vimoption("maxfuncdepth", "mfd", tInt(_)).
vimoption("maxmapdepth", "mmd", tInt(_)).
vimoption("maxmem", "mm", tInt(_)).
vimoption("maxmempattern", "mmp", tInt(_)).
vimoption("maxmemtot", "mmt", tInt(_)).
vimoption("menuitems", "mis", tInt(_)).
vimoption("mkspellmem", "msm", tString(_)).
vimoption("modeline", "ml", tBool(_)).
vimoption("modelines", "mls", tInt(_)).
vimoption("modifiable", "ma", tBool(_)).
vimoption("modified", "mod", tBool(_)).
vimoption("more", "", tBool(_)).
vimoption("mouse", "", tString(_)).
vimoption("mousefocus", "mousef", tBool(_)).
vimoption("mousehide", "mh", tBool(_)).
vimoption("mousemodel", "mousem", tString(_)).
vimoption("mouseshape", "mouses", tString(_)).
vimoption("mousetime", "mouset", tInt(_)).
vimoption("mzschemedll", "", tString(_)).
vimoption("mzschemegcdll", "", tString(_)).
vimoption("mzquantum", "mzq", tInt(_)).
vimoption("nrformats", "nf", tString(_)).
vimoption("tInt(_)", "nu", tBool(_)).
vimoption("numberwidth", "nuw", tInt(_)).
vimoption("omnifunc", "ofu", tString(_)).
vimoption("opendevice", "odev", tBool(_)).
vimoption("operatorfunc", "opfunc", tString(_)).
vimoption("osfiletype", "oft", tString(_)).
vimoption("packpath", "pp", tString(_)).
vimoption("paragraphs", "para", tString(_)).
vimoption("paste", "", tBool(_)).
vimoption("pastetoggle", "pt", tString(_)).
vimoption("patchexpr", "pex", tString(_)).
vimoption("patchmode", "pm", tString(_)).
vimoption("path", "pa", tString(_)).
vimoption("perldll", "", tString(_)).
vimoption("preserveindent", "pi", tBool(_)).
vimoption("previewheight", "pvh", tInt(_)).
vimoption("previewwindow", "pvw", tBool(_)).
vimoption("printdevice", "pdev", tString(_)).
vimoption("printencoding", "penc", tString(_)).
vimoption("printexpr", "pexpr", tString(_)).
vimoption("printfont", "pfn", tString(_)).
vimoption("printheader", "pheader", tString(_)).
vimoption("printmbcharset", "pmbcs", tString(_)).
vimoption("printmbfont", "pmbfn", tString(_)).
vimoption("printoptions", "popt", tString(_)).
vimoption("prompt", "", tBool(_)).
vimoption("pumheight", "ph", tInt(_)).
vimoption("pumwidth", "pw", tInt(_)).
vimoption("pythondll", "", tString(_)).
vimoption("pythonhome", "", tString(_)).
vimoption("pythonthreedll", "", tString(_)).
vimoption("pythonthreehome", "", tString(_)).
vimoption("pyxversion", "pyx", tInt(_)).
vimoption("quoteescape", "qe", tString(_)).
vimoption("readonly", "ro", tBool(_)).
vimoption("redrawtime", "rdt", tInt(_)).
vimoption("regexpengine", "re", tInt(_)).
vimoption("relativenumber", "rnu", tBool(_)).
vimoption("remap", "", tBool(_)).
vimoption("renderoptions", "rop", tString(_)).
vimoption("report", "", tInt(_)).
vimoption("restorescreen", "rs", tBool(_)).
vimoption("revins", "ri", tBool(_)).
vimoption("rightleft", "rl", tBool(_)).
vimoption("rightleftcmd", "rlc", tString(_)).
vimoption("rubydll", "", tString(_)).
vimoption("ruler", "ru", tBool(_)).
vimoption("rulerformat", "ruf", tString(_)).
vimoption("runtimepath", "rtp", tString(_)).
vimoption("scroll", "scr", tInt(_)).
vimoption("scrollbind", "scb", tBool(_)).
vimoption("scrolljump", "sj", tInt(_)).
vimoption("scrolloff", "so", tInt(_)).
vimoption("scrollopt", "sbo", tString(_)).
vimoption("sections", "sect", tString(_)).
vimoption("secure", "", tBool(_)).
vimoption("selection", "sel", tString(_)).
vimoption("selectmode", "slm", tString(_)).
vimoption("sessionoptions", "ssop", tString(_)).
vimoption("shell", "sh", tString(_)).
vimoption("shellcmdflag", "shcf", tString(_)).
vimoption("shellpipe", "sp", tString(_)).
vimoption("shellquote", "shq", tString(_)).
vimoption("shellredir", "srr", tString(_)).
vimoption("shellslash", "ssl", tBool(_)).
vimoption("shelltemp", "stmp", tBool(_)).
vimoption("shelltype", "st", tInt(_)).
vimoption("shellxescape", "sxe", tString(_)).
vimoption("shellxquote", "sxq", tString(_)).
vimoption("shiftround", "sr", tBool(_)).
vimoption("shiftwidth", "sw", tInt(_)).
vimoption("shortmess", "shm", tString(_)).
vimoption("shortname", "sn", tBool(_)).
vimoption("showbreak", "sbr", tString(_)).
vimoption("showcmd", "sc", tBool(_)).
vimoption("showfulltag", "sft", tBool(_)).
vimoption("showmatch", "sm", tBool(_)).
vimoption("showmode", "smd", tBool(_)).
vimoption("showtabline", "stal", tInt(_)).
vimoption("sidescroll", "ss", tInt(_)).
vimoption("sidescrolloff", "siso", tInt(_)).
vimoption("signcolumn", "scl", tString(_)).
vimoption("smartcase", "scs", tBool(_)).
vimoption("smartindent", "si", tBool(_)).
vimoption("smarttab", "sta", tBool(_)).
vimoption("softtabstop", "sts", tInt(_)).
vimoption("spell", "", tBool(_)).
vimoption("spellcapcheck", "spc", tString(_)).
vimoption("spellfile", "spf", tString(_)).
vimoption("spelllang", "spl", tString(_)).
vimoption("spellsuggest", "sps", tString(_)).
vimoption("splitbelow", "sb", tBool(_)).
vimoption("splitright", "spr", tBool(_)).
vimoption("startofline", "sol", tBool(_)).
vimoption("statusline", "stl", tString(_)).
vimoption("suffixes", "su", tString(_)).
vimoption("suffixesadd", "sua", tString(_)).
vimoption("swapfile", "swf", tBool(_)).
vimoption("swapsync", "sws", tString(_)).
vimoption("switchbuf", "swb", tString(_)).
vimoption("synmaxcol", "smc", tInt(_)).
vimoption("syntax", "syn", tString(_)).
vimoption("tabline", "tal", tString(_)).
vimoption("tabpagemax", "tpm", tInt(_)).
vimoption("tabstop", "ts", tInt(_)).
vimoption("tagbsearch", "tbs", tBool(_)).
vimoption("tagcase", "tc", tString(_)).
vimoption("taglength", "tl", tInt(_)).
vimoption("tagrelative", "tr", tBool(_)).
vimoption("tags", "tag", tString(_)).
vimoption("tagstack", "tgst", tBool(_)).
vimoption("tcldll", "", tString(_)).
vimoption("term", "", tString(_)).
vimoption("termbidi", "tbidi", tBool(_)).
vimoption("termencoding", "tenc", tString(_)).
vimoption("termguicolors", "tgc", tBool(_)).
vimoption("termwinscroll", "twsl", tInt(_)).
vimoption("termwinkey", "twk", tString(_)).
vimoption("termwinsize", "tws", tString(_)).
vimoption("terse", "", tBool(_)).
vimoption("textauto", "ta", tBool(_)).
vimoption("textmode", "tx", tBool(_)).
vimoption("textwidth", "tw", tInt(_)).
vimoption("thesaurus", "tsr", tString(_)).
vimoption("tildeop", "top", tBool(_)).
vimoption("timeout", "to", tBool(_)).
vimoption("ttimeout", "", tBool(_)).
vimoption("timeoutlen", "tm", tInt(_)).
vimoption("ttimeoutlen", "ttm", tInt(_)).
vimoption("title", "", tBool(_)).
vimoption("titlelen", "", tInt(_)).
vimoption("titleold", "", tString(_)).
vimoption("titlestring", "", tString(_)).
vimoption("toolbar", "tb", tString(_)).
vimoption("toolbariconsize", "tbis", tString(_)).
vimoption("ttybuiltin", "tbi", tBool(_)).
vimoption("ttyfast", "tf", tBool(_)).
vimoption("ttymouse", "ttym", tString(_)).
vimoption("ttyscroll", "tsl", tInt(_)).
vimoption("ttytype", "tty", tString(_)).
vimoption("undodir", "udir", tString(_)).
vimoption("undofile", "udf", tBool(_)).
vimoption("undolevels", "ul", tInt(_)).
vimoption("undoreload", "ur", tInt(_)).
vimoption("updatecount", "uc", tInt(_)).
vimoption("updatetime", "ut", tInt(_)).
vimoption("varsofttabstop", "vsts", tString(_)).
vimoption("vartabstop", "vts", tString(_)).
vimoption("verbose", "vbs", tInt(_)).
vimoption("verbosefile", "vfile", tString(_)).
vimoption("viewdir", "vdir", tString(_)).
vimoption("viewoptions", "vop", tString(_)).
vimoption("viminfo", "vi", tString(_)).
vimoption("viminfofile", "vif", tString(_)).
vimoption("virtualedit", "ve", tString(_)).
vimoption("visualbell", "vb", tBool(_)).
vimoption("warn", "", tBool(_)).
vimoption("weirdinvert", "wiv", tBool(_)).
vimoption("whichwrap", "ww", tString(_)).
vimoption("wildchar", "wc", tInt(_)).
vimoption("wildcharm", "wcm", tInt(_)).
vimoption("wildignore", "wig", tString(_)).
vimoption("wildignorecase", "wic", tBool(_)).
vimoption("wildmenu", "wmnu", tBool(_)).
vimoption("wildmode", "wim", tString(_)).
vimoption("wildoptions", "wop", tString(_)).
vimoption("winaltkeys", "wak", tString(_)).
vimoption("window", "wi", tInt(_)).
vimoption("winheight", "wh", tInt(_)).
vimoption("winfixheight", "wfh", tBool(_)).
vimoption("winfixwidth", "wfw", tBool(_)).
vimoption("winminheight", "wmh", tInt(_)).
vimoption("winminwidth", "wmw", tInt(_)).
vimoption("winptydll", "", tString(_)).
vimoption("winwidth", "wiw", tInt(_)).
vimoption("wrap", "", tBool(_)).
vimoption("wrapmargin", "wm", tInt(_)).
vimoption("wrapscan", "ws", tBool(_)).
vimoption("write", "", tBool(_)).
vimoption("writeany", "wa", tBool(_)).
vimoption("writebackup", "wb", tBool(_)).
vimoption("writedelay", "wd", tInt(_)).

% ===================== Built-in variables =====================

% vimvar(Scope, Name, Flags, Type)
vimvar("a", "", [vvRO], tDict(_)).
vimvar("a", "0", [vvRO], tInt(_)).
vimvar("a", "000", [vvRO], tList(_)).
vimvar("a", "firstline", [vvRO], tInt(_)).
vimvar("a", "lastline", [vvRO], tInt(_)).
vimvar("b", "", [vvRO], tDict(_)).
vimvar("b", "changedtick", [vvRO], tInt(_)).
vimvar("g", "", [vvRO], tDict(_)).
vimvar("l", "", [vvRO], tDict(_)).
vimvar("s", "", [vvRO], tDict(_)).
vimvar("t", "", [vvRO], tDict(_)).
vimvar("v", "", [vvRO], tDict(_)).
vimvar("v", "beval_bufnr", [vvRO], tInt(_)).
vimvar("v", "beval_col", [vvRO], tInt(_)).
vimvar("v", "beval_lnum", [vvRO], tInt(_)).
vimvar("v", "beval_text", [vvRO], tString(_)).
vimvar("v", "beval_winid", [vvRO], tInt(_)).
vimvar("v", "beval_winnr", [vvRO], tInt(_)).
vimvar("v", "char", [], tString(_)).
vimvar("v", "charconvert_from", [vvRO], tString(_)).
vimvar("v", "charconvert_to", [vvRO], tString(_)).
vimvar("v", "cmdarg", [vvRO], tString(_)).
vimvar("v", "cmdbang", [vvRO], tInt(_)).
vimvar("v", "completed_item", [vvRO], tDict(_)).
vimvar("v", "count", [vvCompat, vvRO], tInt(_)).
vimvar("v", "count1", [vvRO], tInt(_)).
vimvar("v", "ctype", [vvRO], tString(_)).
vimvar("v", "dying", [vvRO], tInt(_)).
vimvar("v", "errmsg", [vvCompat], tString(_)).
vimvar("v", "errors", [], tList(_)).
vimvar("v", "event", [vvRO], tDict(_)).
vimvar("v", "exception", [vvRO], tString(_)).
vimvar("v", "false", [vvRO], tBool(false)).
vimvar("v", "fcs_choice", [], tString(_)).
vimvar("v", "fcs_reason", [vvRO], tString(_)).
vimvar("v", "fname", [vvRO], tString(_)).
vimvar("v", "fname_diff", [vvRO], tString(_)).
vimvar("v", "fname_in", [vvRO], tString(_)).
vimvar("v", "fname_new", [vvRO], tString(_)).
vimvar("v", "fname_out", [vvRO], tString(_)).
vimvar("v", "folddashes", [vvROSBX], tString(_)).
vimvar("v", "foldend", [vvROSBX], tInt(_)).
vimvar("v", "foldlevel", [vvROSBX], tInt(_)).
vimvar("v", "foldstart", [vvROSBX], tInt(_)).
vimvar("v", "hlsearch", [], tInt(_)).
vimvar("v", "insertmode", [vvRO], tString(_)).
vimvar("v", "key", [vvRO], tString(_)).
vimvar("v", "lang", [vvRO], tString(_)).
vimvar("v", "lc_time", [vvRO], tString(_)).
vimvar("v", "lnum", [vvROSBX], tInt(_)).
vimvar("v", "mouse_col", [], tInt(_)).
vimvar("v", "mouse_lnum", [], tInt(_)).
vimvar("v", "mouse_win", [], tInt(_)).
vimvar("v", "mouse_winid", [], tInt(_)).
vimvar("v", "none", [vvRO], tNone(none)).
vimvar("v", "null", [vvRO], tNone(null)).
vimvar("v", "oldfiles", [], tList(_)).
vimvar("v", "operator", [vvRO], tString(_)).
vimvar("v", "option_new", [vvRO], tString(_)).
vimvar("v", "option_old", [vvRO], tString(_)).
vimvar("v", "option_type", [vvRO], tString(_)).
vimvar("v", "prevcount", [vvRO], tInt(_)).
vimvar("v", "profiling", [vvRO], tInt(_)).
vimvar("v", "progname", [vvRO], tString(_)).
vimvar("v", "progpath", [vvRO], tString(_)).
vimvar("v", "register", [vvRO], tString(_)).
vimvar("v", "scrollstart", [], tString(_)).
vimvar("v", "searchforward", [], tInt(_)).
vimvar("v", "servername", [vvRO], tString(_)).
vimvar("v", "shell_error", [vvCompat, vvRO], tInt(_)).
vimvar("v", "statusmsg", [], tString(_)).
vimvar("v", "swapchoice", [], tString(_)).
vimvar("v", "swapcommand", [vvRO], tString(_)).
vimvar("v", "swapname", [vvRO], tString(_)).
vimvar("v", "t_bool", [vvRO], tInt(6)).
vimvar("v", "t_channel", [vvRO], tInt(9)).
vimvar("v", "t_dict", [vvRO], tInt(4)).
vimvar("v", "t_float", [vvRO], tInt(5)).
vimvar("v", "t_func", [vvRO], tInt(2)).
vimvar("v", "t_job", [vvRO], tInt(8)).
vimvar("v", "t_list", [vvRO], tInt(3)).
vimvar("v", "t_none", [vvRO], tInt(7)).
vimvar("v", "t_number", [vvRO], tInt(0)).
vimvar("v", "t_string", [vvRO], tInt(1)).
vimvar("v", "termblinkresp", [vvRO], tString(_)).
vimvar("v", "termrbgresp", [vvRO], tString(_)).
vimvar("v", "termresponse", [vvRO], tString(_)).
vimvar("v", "termrfgresp", [vvRO], tString(_)).
vimvar("v", "termstyleresp", [vvRO], tString(_)).
vimvar("v", "termu7resp", [vvRO], tString(_)).
vimvar("v", "testing", [], tInt(_)).
vimvar("v", "this_session", [vvCompat], tString(_)).
vimvar("v", "throwpoint", [vvRO], tString(_)).
vimvar("v", "true", [vvRO], tBool(true)).
vimvar("v", "val", [vvRO], _).
vimvar("v", "version", [vvCompat, vvRO], tInt(_)).
vimvar("v", "vim_did_enter", [vvRO], tInt(_)).
vimvar("v", "warningmsg", [], tString(_)).
vimvar("v", "windowid", [vvRO], tInt(_)).
vimvar("w", "", [vvRO], tDict(_)).

% Some bare words become v: variables for compatibility (e.g. "count" => "v:count")
compat(Name) :- vimvar("v", Name, Flags, _), member(vvCompat, Flags).

% ===================== Env =====================

% eval(+Node, -RetEnv, -Result)
eval(Node, RetEnv, Result) :-
  new_eval_env(Env),
  eval(Env, Node, RetEnv, Result), !.

% ----------------- private -----------------

eval(Env, Node, RetEnv, Result) :-
  traverse(Env, Node, E1),
  get_result(E1, Result, RetEnv), !.

get_result(Env, Result, RetEnv) :-
  empty_errors(Env), !,
  pop(Env, Result, RetEnv).
get_result(Env, Result, RetEnv) :-
  pop_error(Env, Result, RetEnv).

% new_eval_env(-Env)
new_eval_env(Env) :-
  new_env(E1),
  add_hooks(E1, [on_enter:on_function, on_leave:on_function, on_enter:on_let_enter, on_leave:eval_node], E2),
  append(E2, [stack:[tSuccess], lv:0, vars:[], funcs:[], errors:[]], Env), !.

get_stack(Env, Stack) :- member(stack:Stack, Env).

% push(+Env, +Value, -RetEnv)
push(Env, Value, RetEnv) :-
  update_assoc_list(stack:Stack, Env, stack:[Value | Stack], RetEnv).

% pop(+Env, -Value, -RetEnv)
pop(Env, Value, RetEnv) :-
  update_assoc_list(stack:[Value | Rest], Env, stack:Rest, RetEnv).

% get_level(+Env, -Lv)
get_level(Env, Lv) :- member(lv:Lv, Env).

% succ_level(+Env, -RetEnv)
succ_level(Env, RetEnv) :-
  update_assoc_list(lv:Lv, Env, Lv:Lv_, RetEnv), succ(Lv, Lv_), !.
% pred_level(+Env, -RetEnv)
pred_level(Env, RetEnv) :-
  update_assoc_list(lv:Lv, Env, Lv:Lv_, RetEnv), pred(Lv, Lv_), !.

% get_vars(+Env, -Vars)
get_vars(Env, Vars) :- member(vars:Vars, Env).

% update_var(+Env, +Vars, +UpdateVars, -RetEnv)
update_var(Env, Vars, UpdateVars, RetEnv) :-
  update_assoc_list(vars:Vars, Env, vars:UpdateVars, RetEnv).

% get_funcs(+Env, -Funcs)
get_funcs(Env, Funcs) :- member(funcs:Funcs, Env).

% update_func(+Env, +Funcs, +UpdateFuncs, -RetEnv)
update_func(Env, Funcs, UpdateFuncs, RetEnv) :-
  update_assoc_list(funcs:Funcs, Env, funcs:UpdateFuncs, RetEnv).

% add_func(+Env, function(+Name, +Params, +Body) @ +Pos, -RetEnv)
add_func(Env, function(Name, Params, Body) @ Pos, RetEnv) :-
  add_params(Env, Params, Env1),
  update_func(Env1, Funcs, [function(Name, Params, Body) @ Pos | Funcs], RetEnv).

get_errors(Env, Errs) :- member(errors:Errs, Env).

empty_errors(Env) :- get_errors(Env, []).

% add_errors(+Env, +Errs, -RetEnv)
add_errors(Env, Errs, RetEnv) :-
  append(Errs, Errors, Added),
  update_assoc_list(errors:Errors, Env, errors:Added, RetEnv), !.

% pop_errors(+Env, -Err, -RetEnv)
pop_error(Env, Err, RetEnv) :-
  update_assoc_list(errors:[Err | Xs], Env, errors:Xs, RetEnv), !.

% add_params(+Env, +Params, -RetEnv)
add_params(Env, Params, RetEnv) :-
  foldl(add_param1, Params, Env, RetEnv).
add_param1(ident("", Name) @ Pos, Env, RetEnv) :-
  add_var(Env, ident("a", Name) @ Pos, _, RetEnv).

% call_func(+Env, call(+Fun, +Args), -R)
call_func(_, call(Args :: R @ _, Args), R).
% No scope, must be a Vim built-in function or a variable (e.g. has("eval"), F(42))
call_func(Env, call(ident("", Name) @ _, Args), R) :-
  vimfunc(Env, Name, Args :: R @ _), !;
  add_scope(Env, Name, Scope),
  call_func(Env, call(ident(Scope, Name) @ _, Args), R), !.
% A variable (with scope) is a funcref (e.g. l:F(42), g:F(42))
call_func(Env, call(ident(Scope, Name) @ _, Args), R) :-
  \+ Scope = "",
  eval(Env, ident(Scope, Name) @ _, _, FunT @ _),
  call_func(Env, call(FunT @ _, Args), R), !.
% Expression is a funcref
%
%		" function('has') = call(ident(Scope, Name) @ _, InnerArgs) @ _
%		call function('has')('eval')
%
call_func(Env, call(call(ident(Scope, Name) @ _, InnerArgs) @ _, Args), R) :-
  call_func(Env, call(ident(Scope, Name) @ _, InnerArgs), FunT),
  call_func(Env, call(FunT @ _, Args), R).

% add_var(+Env, ident(+Scope, +Name) @ +Pos, +Rhs, -RetEnv)
add_var(Env, ident("", Name) @ Pos, Rhs, RetEnv) :-
  add_scope(Env, Name, Scope),
  add_var(Env, ident(Scope, Name) @ Pos, Rhs, RetEnv).
add_var(Env, ident(Scope, Name) @ Pos, Rhs, RetEnv) :-
  \+ Scope = "",
  update_var(Env, Vars, [ident(Scope, Name) @ Pos => Rhs | Vars], RetEnv).

% get_var(+Env, ident(?Scope, ?Name), -Pos, -Rhs)
%
% Look up ident variable from env.
% variables and functions have different namespace.
% For example, below code outputs "1", and "42".
%
%		let has = 42
%		echo has("eval")
%		echo has
%
% See call_func/3 for the look-up of functions.
%
get_var(Env, ident(Scope, Name), Pos, Rhs) :-
  add_scope_nonvar(Env, Name, Scope, Scope1),
  get_vars(Env, Vars),
  member(ident(Scope1, Name) @ Pos => Rhs, Vars).

add_scope_nonvar(Env, Name, Scope, Scope_) :-
  nonvar(Name), nonvar(Scope), Scope = "", !,
  add_scope(Env, Name, Scope_), !.
add_scope_nonvar(_, _, Scope, Scope) :- !.

% add_scope(+Env, +Name, -Scope)
add_scope(_, Name, "v") :- compat(Name), !.
add_scope(Env, _, "l") :- get_level(Env, Lv), Lv > 0, !.
add_scope(_, _, "g") :- !.

% get_func(+Env, ident(?Scope, ?Name), ?Pos, ?Func)
get_func(Env, ident(Scope, Name), Pos, Func) :-
  get_funcs(Env, Funcs),
  Func = function(ident(Scope, Name) @ _, _, _),
  member(Func @ Pos, Funcs).

% ===================== Evaluation functions =====================

% Add variable to Env
% TODO destructuring, subscript, dot, ...
on_let_enter(Env, let(Lhs, =, Rhs) @ _, on_enter, RetEnv) :-
  add_var(Env, Lhs, Rhs, RetEnv), !.

% Add function to Env
on_function(Env, function(Name, Params, Body) @ Pos, on_enter, RetEnv) :-
  !, add_func(Env, function(Name, Params, Body) @ Pos, E1),
 succ_level(E1, RetEnv), !.
on_function(Env, function(_, _, _) @ _, on_leave, RetEnv) :-
  !, pred_level(Env, RetEnv), !.

eval_node(Env, Node, on_leave, RetEnv) :-
  get_stack(Env, Stack),
  reduce(Env, Node, Stack, RetStack, Errs), !,
  update_assoc_list(stack:Stack, Env, stack:RetStack, E1),
  add_errors(E1, Errs, RetEnv), !.
eval_node(Env, _, on_leave, Env) :- !.    % reduce/5 was failed

% ------- reduce(+Env, +Node, +Stack, -RetStack) -------

:- discontiguous(reduce/5).

% Primitive values.
reduce(_, Node @ Pos, Stack, RetStack, Errs) :-
  prim(Node), !,
  reduce_prim(Node @ Pos, Stack, RetStack, Errs), !.

reduce_prim(tList(L) @ Pos, Stack, RetStack, []) :-
  !, reduce_list(L, Stack, L1, Stack1),
  RetStack = [tList(L1) @ Pos | Stack1], !.

reduce_prim(tTuple(L) @ Pos, Stack, RetStack, []) :-
  !, reduce_list(L, Stack, L1, Stack1),
  RetStack = [tTuple(L1) @ Pos | Stack1], !.

reduce_prim(tDict(Entries) @ Pos, Stack, RetStack, Errs) :-
  !, reduce_dict(tDict(Entries) @ Pos, Stack, RetStack, Errs), !.

reduce_prim(Node @ Pos, Stack, [Node @ Pos | Stack], []) :- !.

reduce_list(L, Stack, Taken, RetStack) :-
  length(L, N), split_list(N, Stack, Taken, RetStack), !.

reduce_dict(tDict(Entries) @ Pos, Stack, RetStack, Errs) :-
  length(Entries, EntriesN), N is EntriesN * 2,
  split_list(N, Stack, KeyValues, Stack1),
  reduce_key_values(KeyValues, Entries1, Errs),
  RetStack = [tDict(Entries1) @ Pos | Stack1], !.

reduce_key_values([], [], []) :- !.
reduce_key_values([V, tString(K) @ Pos | Xs], [tString(K) @ Pos : V | Entries], Errs) :-
  !, reduce_key_values(Xs, Entries, Errs), !.
reduce_key_values([_, _ @ Pos | _], [], [Err @ Pos]) :-
  !, error('key is not string', [], Err), !.

reduce(_, file(_) @ _, Stack, Stack, []) :- !.

reduce(_, comment(_) @ _, Stack, Stack, []) :- !.

reduce(_, excmd(_) @ _, Stack, Stack, []) :- !.

reduce(_, return(_) @ _, [_ | Stack], Stack, []) :- !.

reduce(_, function(_, ParamsOrig, _) @ _, Stack, RetStack, []) :-
  length(ParamsOrig, ParamsN),
  N is 1 + ParamsN,    % Name + Params
  split_list(N, Stack, _, RetStack), !.

reduce(_, excall(_) @ _, [_ | Stack], Stack, []) :- !.

% TODO destructuring, subscript, dot, ...
reduce(_, let(_, _, _) @ _, [_, _ | Stack], Stack, []) :- !.

reduce(_, if(_, _) @ _, [_ | Stack], Stack, []) :- !.

reduce(_, echo(ExprList) @ _, Stack, RetStack, []) :-
  length(ExprList, N),
  split_list(N, Stack, _, RetStack), !.

reduce(_, subscript(_, _) @ Pos, [Right @ _, Left @ _ | Stack], [Value @ Pos | Stack], []) :-
  get_prop(Left, Right, Value), !.

reduce(_, dot(_, _) @ Pos, [ident(_, Name) @ _, Left @ _ | Stack], [Value @ Pos | Stack], []) :-
  get_prop(Left, tString(Name), Value), !.

% TODO check lhs, rhs types
% TODO other ops
reduce(_, op(_, ==, _) @ Pos, [_, _ | Stack], [tInt(_) @ Pos | Stack], []) :- !.

reduce(Env, call(_, ArgsOrig) @ Pos, Stack, RetStack, []) :-
  length(ArgsOrig, Arity),
  N is 1 + Arity,    % Fun + Args
  split_list(N, Stack, [Fun | Args], S1),
  call_func(Env, call(Fun, Args), Value),
  RetStack = [Value @ Pos | S1], !.

% reduce(+Env, ident(+Scope, +Name) @ +Pos, -RetEnv)
reduce(_, ident(Scope, Name) @ Pos, Stack, [ident(Scope, Name) @ Pos, Stack], []) :- !.

% reduce(+Env, option(+Name) @ +Pos, -RetEnv)
reduce(_, option(Name) @ Pos, Stack, [Value @ Pos | Stack], []) :-
  vimoption(Name, Value), !;
  error('no such option: ~s', [Name], Value), !.

% reduce(+Env, env(+Name) @ +Pos, -RetEnv)
reduce(_, env(_) @ Pos, Stack, [tString(_) @ Pos | Stack], []) :- !.

% reduce(+Env, reg(+Name) @ +Pos, -RetEnv)
reduce(_, reg(_) @ Pos, Stack, [tString(_) @ Pos | Stack], []) :- !.

% ===================== Traversal functions =====================

% new_env(-Env)
new_env([hooks:[]]).

% get_hooks(+Env, -Hooks)
get_hooks(Env, Hooks) :- member(hooks:Hooks, Env).
% add_hooks(+Env, +FuncList, -RetEnv)
add_hooks(Env, FuncList, RetEnv) :-
  update_assoc_list(hooks:Hooks, Env, hooks:Added, RetEnv),
  append(FuncList, Hooks, Added).

% run_hook(+Env, +Node, +Event, -RetEnv)
run_hook(Env, Node, Event, RetEnv) :-
  get_hooks(Env, Hooks),
  findall(Func, member(Event:Func, Hooks), FuncList),
  foldl(do_run_hook(Node, Event), FuncList, Env, RetEnv).
do_run_hook(Node, Event, Func, Env, RetEnv) :-
  call(Func, Env, Node, Event, RetEnv), !.
do_run_hook(_, _, _, Env, Env) :- !.

% traverse(+Env, +Node, -RetEnv)
traverse(Env, Node, RetEnv) :-
  run_hook(Env, Node, on_enter, E1),
  trav(E1, Node, E2),
  run_hook(E2, Node, on_leave, RetEnv).

% traverse_list(+Env, +NodeList, -RetEnv)
traverse_list(Env, NodeList, RetEnv) :- foldl(traverse_rev, NodeList, Env, RetEnv).
traverse_rev(Node, Env, RetEnv) :- traverse(Env, Node, RetEnv).

:- discontiguous(trav/3).

% Primitive types
trav(Env, Node @ _, RetEnv) :-
  prim(Node), !,
  trav_prim(Env, Node, RetEnv), !.

trav_prim(Env, tList(L), RetEnv) :- !, traverse_list(Env, L, RetEnv), !.
trav_prim(Env, tTuple(L), RetEnv) :- !, traverse_list(Env, L, RetEnv), !.
trav_prim(Env, tDict(Entries), RetEnv) :- !, traverse_entries(Env, Entries, RetEnv), !.
trav_prim(Env, _, Env) :- !.

traverse_entries(Env, Entries, RetEnv) :- foldl(traverse_entry, Entries, Env, RetEnv).
traverse_entry(Key:Value, Env, RetEnv) :- traverse(Env, Key, E1), traverse(E1, Value, RetEnv).

% trav(+Env, file(+Excmds) @ +Pos, -RetEnv)
trav(Env, file(Body) @ _, RetEnv) :- traverse_list(Env, Body, RetEnv), !.

% TODO type comment
% trav(+Env, comment(+Text) @ +Pos, -RetEnv)
trav(Env, comment(_) @ _, Env) :- !.

% TODO analyze arguments of excmd
% trav(+Env, excmd(+Command) @ +Pos, -RetEnv)
trav(Env, excmd(_) @ _, Env) :- !.

% trav(+Env, return(+Expr) @ +Pos, -RetEnv)
trav(Env, return(Expr) @ _, RetEnv) :- traverse(Env, Expr, RetEnv), !.

% trav(+Env, function(+Name, +Params, +Body) @ +Pos, -RetEnv)
trav(Env, function(Name, Params, Body) @ _, RetEnv) :-
  append([Name | Params], Body, L),
  traverse_list(Env, L, RetEnv), !.

% trav(+Env, excall(+FuncCall) @ +Pos, -RetEnv)
trav(Env, excall(FuncCall) @ _, RetEnv) :- traverse(Env, FuncCall, RetEnv), !.

% trav(+Env, let(+Lhs, +Op, +Rhs) @ +Pos, -RetEnv)
trav(Env, let(Lhs, _, Rhs) @ _, RetEnv) :- traverse_list(Env, [Lhs, Rhs], RetEnv), !.

% trav(+Env, if(+Cond, +Body) @ +Pos, -RetEnv)
trav(Env, if(Cond, Body) @ _, RetEnv) :- traverse_list(Env, [Cond | Body], RetEnv), !.

% TODO
% trav(+Env, if(+Cond, +Body, else(+ElseBody)) @ +Pos, -RetEnv)
% trav(+Env, if(+Cond, +Body, elseif([+ElseCond, +ElseIfBody, ...])) @ +Pos, -RetEnv)
% trav(+Env, if(+Cond, +Body, elseif([+ElseCond, +ElseIfBody, ...]), else(+ElseBody)) @ +Pos, -RetEnv)

% trav(+Env, echo(+ExprList) @ +Pos, -RetEnv)
trav(Env, echo(ExprList) @ _, RetEnv) :- traverse_list(Env, ExprList, RetEnv), !.

% TODO
% trav(+Env, subscript(+Left, +Right) @ +Pos, -RetEnv)
trav(Env, subscript(Left, Right) @ _, RetEnv) :- traverse_list(Env, [Left, Right], RetEnv), !.

% trav(+Env, dot(+Left, +Right) @ +Pos, -RetEnv)
trav(Env, dot(Left, Right) @ _, RetEnv) :- traverse_list(Env, [Left, Right], RetEnv), !.

% trav(+Env, op(+Left, +Op, +Right) @ +Pos, -RetEnv)
trav(Env, op(Left, _, Right) @ _, RetEnv) :- traverse_list(Env, [Left, Right], RetEnv), !.

% trav(+Env, call(+Fun, +Args) @ +Pos, -RetEnv)
trav(Env, call(Fun, Args) @ _, RetEnv) :- traverse_list(Env, [Fun | Args], RetEnv), !.

% trav(+Env, ident(+Scope, +Name) @ +Pos, -RetEnv)
trav(Env, ident(_, _) @ _, Env) :- !.

% trav(+Env, option(+Name) @ +Pos, -RetEnv)
trav(Env, option(_) @ _, Env) :- !.

% trav(+Env, env(+Name) @ +Pos, -RetEnv)
trav(Env, env(_) @ _, Env) :- !.

% trav(+Env, reg(+Name) @ +Pos, -RetEnv)
trav(Env, reg(_) @ _, Env) :- !.
