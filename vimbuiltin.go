package main

import (
	"sort"
	"strings"
)

// TODO make these information updatable by users.

var vimFunctions = []vimFunc{
	{"abs", func() vimType {
		return newUnionType(
			newFuncType(newTupleType(typeInt), typeInt),
			newFuncType(newTupleType(typeFloat), typeFloat),
		)
	}},
	{"acos", func() vimType {
		return newFuncType(newTupleType(typeFloat), typeFloat)
	}},
	{"add", func() vimType {
		// TODO generics
		return newFuncType(newTupleType(typeList, newTypeVar()), typeList)
	}},
	{"and", func() vimType {
		return newFuncType(newTupleType(typeInt), typeInt)
	}},
	{"append", func() vimType {
		lnum := newLnumType()
		return newFuncType(newTupleType(lnum, newUnionType(typeList, typeString)), typeInt)
	}},
	{"appendbufline", func() vimType {
		bufexpr := newBufExprType()
		lnum := newLnumType()
		return newFuncType(newTupleType(bufexpr, lnum, newUnionType(typeList, typeString)), typeInt)
	}},
	{"argc", func() vimType {
		return newFuncType(newTupleType(), typeInt)
	}},
	{"argidx", func() vimType {
		return newFuncType(newTupleType(), typeInt)
	}},
	{"arglistid", func() vimType {
		winnr := newWinnrType()
		tabnr := newTabnrType()
		return newUnionType(
			newFuncType(newTupleType(), typeInt),
			newFuncType(newTupleType(winnr), typeInt),
			newFuncType(newTupleType(winnr, tabnr), typeInt),
		)
	}},
	{"argv", func() vimType {
		return newUnionType(
			newFuncType(newTupleType(), typeInt),
			newFuncType(newTupleType(typeInt), typeInt),
		)
	}},
	{"assert_beeps", func() vimType {
		return newFuncType(newTupleType(typeString), typeInt)
	}},
	{"assert_equal", func() vimType {
		a := newTypeVar()
		return newUnionType(
			newFuncType(newTupleType(a, a), typeInt),
			newFuncType(newTupleType(a, a, typeString), typeInt),
		)
	}},
	{"assert_equalfile", func() vimType {
		return newFuncType(newTupleType(typeString, typeString), typeInt)
	}},
	{"assert_exception", func() vimType {
		return newUnionType(
			newFuncType(newTupleType(typeString), typeInt),
			newFuncType(newTupleType(typeString, typeString), typeInt),
		)
	}},
	{"assert_fails", func() vimType {
		return newUnionType(
			newFuncType(newTupleType(typeString), typeInt),
			newFuncType(newTupleType(typeString, typeString), typeInt),
		)
	}},
	{"assert_false", func() vimType {
		return newUnionType(
			newFuncType(newTupleType(typeBoolean), typeInt),
			newFuncType(newTupleType(typeBoolean, typeString), typeInt),
		)
	}},
	{"assert_inrange", func() vimType {
		return newUnionType(
			newFuncType(newTupleType(typeFloat, typeFloat, typeFloat), typeInt),
			newFuncType(newTupleType(typeFloat, typeFloat, typeFloat, typeString), typeInt),
		)
	}},
	{"assert_match", func() vimType {
		pat := newPatternType()
		return newUnionType(
			newFuncType(newTupleType(pat, typeString), typeInt),
			newFuncType(newTupleType(pat, typeString, typeString), typeInt),
		)
	}},
	{"assert_notequal", func() vimType {
		a := newTypeVar()
		return newUnionType(
			newFuncType(newTupleType(a, a), typeInt),
			newFuncType(newTupleType(a, a, typeString), typeInt),
		)
	}},
	{"assert_notmatch", func() vimType {
		pat := newPatternType()
		return newUnionType(
			newFuncType(newTupleType(pat, typeString), typeInt),
			newFuncType(newTupleType(pat, typeString, typeString), typeInt),
		)
	}},
	{"assert_report", func() vimType {
		return newFuncType(newTupleType(typeString), typeInt)
	}},
	{"assert_true", func() vimType {
		return newFuncType(newTupleType(typeBoolean), typeInt)
	}},
	{"asin", func() vimType {
		return newFuncType(newTupleType(typeFloat), typeFloat)
	}},
	{"atan", func() vimType {
		return newFuncType(newTupleType(typeFloat), typeFloat)
	}},
	{"atan2", func() vimType {
		return newFuncType(newTupleType(typeFloat, typeFloat), typeFloat)
	}},
	{"balloon_show", func() vimType {
		return newFuncType(newTupleType(
			newUnionType(typeString, &composeType{typeList, typeString}),
		), typeInt)
	}},
	{"balloon_split", func() vimType {
		return newFuncType(newTupleType(typeString), &composeType{typeList, typeString})
	}},
	{"browse", func() vimType {
		return newFuncType(newTupleType(typeBoolean, typeString, typeString, typeString), typeString)
	}},
	{"browsedir", func() vimType {
		return newFuncType(newTupleType(typeString, typeString), typeString)
	}},
	{"bufexists", func() vimType {
		bufexpr := newBufExprType()
		return newFuncType(newTupleType(bufexpr), typeInt)
	}},
	{"buflisted", func() vimType {
		bufexpr := newBufExprType()
		return newFuncType(newTupleType(bufexpr), typeInt)
	}},
	{"bufloaded", func() vimType {
		bufexpr := newBufExprType()
		return newFuncType(newTupleType(bufexpr), typeInt)
	}},
	{"bufname", func() vimType {
		bufexpr := newBufExprType()
		return newFuncType(newTupleType(bufexpr), typeString)
	}},
	{"bufnr", func() vimType {
		bufexpr := newBufExprType()
		return newUnionType(
			newFuncType(newTupleType(bufexpr), typeString),
			newFuncType(newTupleType(bufexpr, typeBoolean), typeString),
		)
	}},
	{"bufwinid", func() vimType {
		bufexpr := newBufExprType()
		winnr := newWinnrType()
		return newFuncType(newTupleType(bufexpr), winnr)
	}},
	{"bufwinnr", func() vimType {
		bufexpr := newBufExprType()
		winnr := newWinnrType()
		return newFuncType(newTupleType(bufexpr), winnr)
	}},
	{"byte2line", func() vimType {
		return newFuncType(newTupleType(typeInt), typeInt)
	}},
	{"byteidx", func() vimType {
		return newFuncType(newTupleType(typeString, typeInt), typeInt)
	}},
	{"byteidxcomp", func() vimType {
		return newFuncType(newTupleType(typeString, typeInt), typeInt)
	}},
	{"call", func() vimType {
		ret := newTypeVar()
		allArgs := make([]vimType, 20)
		for i := range allArgs {
			allArgs[i] = newTypeVar()
		}
		funlist := make([]vimType, 20)
		for i := range funlist {
			arglist := newTupleType(allArgs[:i]...)
			fun := newFuncType(arglist, ret)
			funlist[i] = newFuncType(newTupleType(fun, arglist), ret)
		}
		return newUnionType(funlist[0], funlist[1], funlist[2:]...)
	}},
	{"ceil", func() vimType {
		return newFuncType(newTupleType(typeFloat), typeFloat)
	}},
	{"ch_canread", func() vimType {
		handle := newHandleType()
		return newFuncType(newTupleType(handle), typeInt)
	}},
	{"ch_close", func() vimType {
		handle := newHandleType()
		return newFuncType(newTupleType(handle), typeInt)
	}},
	{"ch_close_in", func() vimType {
		handle := newHandleType()
		return newFuncType(newTupleType(handle), typeInt)
	}},
	{"ch_evalexpr", func() vimType {
		handle := newHandleType()
		options := newChannelOptionsType()
		return newUnionType(
			newFuncType(newTupleType(handle, typeString), typeString),
			newFuncType(newTupleType(handle, typeString, options), typeString),
		)
	}},
	{"ch_evalraw", func() vimType {
		handle := newHandleType()
		options := newChannelOptionsType()
		return newUnionType(
			newFuncType(newTupleType(handle, typeString), typeString),
			newFuncType(newTupleType(handle, typeString, options), typeString),
		)
	}},
	{"ch_getbufnr", func() vimType {
		handle := newHandleType()
		// TODO string literal type: "err" | "out"
		what := typeString
		return newFuncType(newTupleType(handle, what), typeInt)
	}},
	{"ch_getjob", func() vimType {
		return newFuncType(newTupleType(typeChannel), typeJob)
	}},
	{"ch_info", func() vimType {
		handle := newHandleType()
		chInfo := newChannelInfoType()
		return newFuncType(newTupleType(handle), chInfo)
	}},
	{"ch_log", func() vimType {
		handle := newHandleType()
		// TODO return type should be typeVoid?
		return newUnionType(
			newFuncType(newTupleType(typeString), typeInt),
			newFuncType(newTupleType(typeString, handle), typeInt),
		)
	}},
	{"ch_logfile", func() vimType {
		// TODO string literal type: "a" | "w"
		mode := typeString
		return newUnionType(
			newFuncType(newTupleType(typeString), typeInt),
			newFuncType(newTupleType(typeString, mode), typeInt),
		)
	}},
	{"ch_open", func() vimType {
		// :help channel-open-options
		openOptioins := newChannelOpenOptionsType()
		return newUnionType(
			newFuncType(newTupleType(typeString), typeInt),
			newFuncType(newTupleType(typeString, openOptioins), typeInt),
		)
	}},
	{"ch_read", func() vimType {
		handle := newHandleType()
		options := newChannelOptionsType()
		return newUnionType(
			newFuncType(newTupleType(handle), typeString),
			newFuncType(newTupleType(handle, options), typeString),
		)
	}},
	{"ch_readraw", func() vimType {
		handle := newHandleType()
		options := newChannelOptionsType()
		return newUnionType(
			newFuncType(newTupleType(handle), typeString),
			newFuncType(newTupleType(handle, options), typeString),
		)
	}},
	{"ch_sendexpr", func() vimType {
		handle := newHandleType()
		options := newChannelOptionsType()
		return newUnionType(
			newFuncType(newTupleType(handle, typeString), typeString),
			newFuncType(newTupleType(handle, typeString, options), typeString),
		)
	}},
	{"ch_sendraw", func() vimType {
		handle := newHandleType()
		options := newChannelOptionsType()
		return newUnionType(
			newFuncType(newTupleType(handle, typeString), typeString),
			newFuncType(newTupleType(handle, typeString, options), typeString),
		)
	}},
	{"ch_setoptions", func() vimType {
		handle := newHandleType()
		options := newChannelOptionsType()
		return newFuncType(newTupleType(handle, typeString, options), typeInt)
	}},
	{"ch_status", func() vimType {
		return newPossibleFuncType()
	}},
	{"changenr", func() vimType {
		return newPossibleFuncType()
	}},
	{"char2nr", func() vimType {
		return newPossibleFuncType()
	}},
	{"cindent", func() vimType {
		return newPossibleFuncType()
	}},
	{"clearmatches", func() vimType {
		return newPossibleFuncType()
	}},
	{"col", func() vimType {
		return newPossibleFuncType()
	}},
	{"complete", func() vimType {
		return newPossibleFuncType()
	}},
	{"complete_add", func() vimType {
		return newPossibleFuncType()
	}},
	{"complete_check", func() vimType {
		return newPossibleFuncType()
	}},
	{"confirm", func() vimType {
		return newPossibleFuncType()
	}},
	{"copy", func() vimType {
		return newPossibleFuncType()
	}},
	{"cos", func() vimType {
		return newPossibleFuncType()
	}},
	{"cosh", func() vimType {
		return newPossibleFuncType()
	}},
	{"count", func() vimType {
		return newPossibleFuncType()
	}},
	{"cscope_connection", func() vimType {
		return newPossibleFuncType()
	}},
	{"cursor", func() vimType {
		return newPossibleFuncType()
	}},
	{"debugbreak", func() vimType {
		return newPossibleFuncType()
	}},
	{"deepcopy", func() vimType {
		return newPossibleFuncType()
	}},
	{"delete", func() vimType {
		return newPossibleFuncType()
	}},
	{"deletebufline", func() vimType {
		return newPossibleFuncType()
	}},
	{"did_filetype", func() vimType {
		return newPossibleFuncType()
	}},
	{"diff_filler", func() vimType {
		return newPossibleFuncType()
	}},
	{"diff_hlID", func() vimType {
		return newPossibleFuncType()
	}},
	{"empty", func() vimType {
		return newPossibleFuncType()
	}},
	{"escape", func() vimType {
		return newPossibleFuncType()
	}},
	{"eval", func() vimType {
		return newPossibleFuncType()
	}},
	{"eventhandler", func() vimType {
		return newPossibleFuncType()
	}},
	{"executable", func() vimType {
		return newPossibleFuncType()
	}},
	{"execute", func() vimType {
		return newPossibleFuncType()
	}},
	{"exepath", func() vimType {
		return newPossibleFuncType()
	}},
	{"exists", func() vimType {
		return newPossibleFuncType()
	}},
	{"extend", func() vimType {
		return newPossibleFuncType()
	}},
	{"exp", func() vimType {
		return newPossibleFuncType()
	}},
	{"expand", func() vimType {
		return newPossibleFuncType()
	}},
	{"feedkeys", func() vimType {
		return newPossibleFuncType()
	}},
	{"filereadable", func() vimType {
		return newPossibleFuncType()
	}},
	{"filewritable", func() vimType {
		return newPossibleFuncType()
	}},
	{"filter", func() vimType {
		return newPossibleFuncType()
	}},
	{"finddir", func() vimType {
		return newPossibleFuncType()
	}},
	{"findfile", func() vimType {
		return newPossibleFuncType()
	}},
	{"float2nr", func() vimType {
		return newPossibleFuncType()
	}},
	{"floor", func() vimType {
		return newPossibleFuncType()
	}},
	{"fmod", func() vimType {
		return newPossibleFuncType()
	}},
	{"fnameescape", func() vimType {
		return newPossibleFuncType()
	}},
	{"fnamemodify", func() vimType {
		return newPossibleFuncType()
	}},
	{"foldclosed", func() vimType {
		return newPossibleFuncType()
	}},
	{"foldclosedend", func() vimType {
		return newPossibleFuncType()
	}},
	{"foldlevel", func() vimType {
		return newPossibleFuncType()
	}},
	{"foldtext", func() vimType {
		return newPossibleFuncType()
	}},
	{"foldtextresult", func() vimType {
		return newPossibleFuncType()
	}},
	{"foreground", func() vimType {
		return newPossibleFuncType()
	}},
	{"funcref", func() vimType {
		return newPossibleFuncType()
	}},
	{"function", func() vimType {
		return newPossibleFuncType()
	}},
	{"garbagecollect", func() vimType {
		return newPossibleFuncType()
	}},
	{"get", func() vimType {
		return newPossibleFuncType()
	}},
	{"getbufinfo", func() vimType {
		return newPossibleFuncType()
	}},
	{"getbufline", func() vimType {
		return newPossibleFuncType()
	}},
	{"getbufvar", func() vimType {
		return newPossibleFuncType()
	}},
	{"getchangelist", func() vimType {
		return newPossibleFuncType()
	}},
	{"getchar", func() vimType {
		return newPossibleFuncType()
	}},
	{"getcharmod", func() vimType {
		return newPossibleFuncType()
	}},
	{"getcharsearch", func() vimType {
		return newPossibleFuncType()
	}},
	{"getcmdline", func() vimType {
		return newPossibleFuncType()
	}},
	{"getcmdpos", func() vimType {
		return newPossibleFuncType()
	}},
	{"getcmdtype", func() vimType {
		return newPossibleFuncType()
	}},
	{"getcmdwintype", func() vimType {
		return newPossibleFuncType()
	}},
	{"getcompletion", func() vimType {
		return newPossibleFuncType()
	}},
	{"getcurpos", func() vimType {
		return newPossibleFuncType()
	}},
	{"getcwd", func() vimType {
		return newPossibleFuncType()
	}},
	{"getfontname", func() vimType {
		return newPossibleFuncType()
	}},
	{"getfperm", func() vimType {
		return newPossibleFuncType()
	}},
	{"getfsize", func() vimType {
		return newPossibleFuncType()
	}},
	{"getftime", func() vimType {
		return newPossibleFuncType()
	}},
	{"getftype", func() vimType {
		return newPossibleFuncType()
	}},
	{"getjumplist", func() vimType {
		return newPossibleFuncType()
	}},
	{"getline", func() vimType {
		return newPossibleFuncType()
	}},
	{"getloclist", func() vimType {
		return newPossibleFuncType()
	}},
	{"getmatches", func() vimType {
		return newPossibleFuncType()
	}},
	{"getpid", func() vimType {
		return newPossibleFuncType()
	}},
	{"getpos", func() vimType {
		return newPossibleFuncType()
	}},
	{"getqflist", func() vimType {
		return newPossibleFuncType()
	}},
	{"getreg", func() vimType {
		return newPossibleFuncType()
	}},
	{"getregtype", func() vimType {
		return newPossibleFuncType()
	}},
	{"gettabinfo", func() vimType {
		return newPossibleFuncType()
	}},
	{"gettabvar", func() vimType {
		return newPossibleFuncType()
	}},
	{"gettabwinvar", func() vimType {
		return newPossibleFuncType()
	}},
	{"getwininfo", func() vimType {
		return newPossibleFuncType()
	}},
	{"getwinpos", func() vimType {
		return newPossibleFuncType()
	}},
	{"getwinposx", func() vimType {
		return newPossibleFuncType()
	}},
	{"getwinposy", func() vimType {
		return newPossibleFuncType()
	}},
	{"getwinvar", func() vimType {
		return newPossibleFuncType()
	}},
	{"glob", func() vimType {
		return newPossibleFuncType()
	}},
	{"glob2regpat", func() vimType {
		return newPossibleFuncType()
	}},
	{"globpath", func() vimType {
		return newPossibleFuncType()
	}},
	{"has", func() vimType {
		return newFuncType(newTupleType(typeString), typeInt)
	}},
	{"has_key", func() vimType {
		return newPossibleFuncType()
	}},
	{"haslocaldir", func() vimType {
		return newPossibleFuncType()
	}},
	{"hasmapto", func() vimType {
		return newPossibleFuncType()
	}},
	{"histadd", func() vimType {
		return newPossibleFuncType()
	}},
	{"histdel", func() vimType {
		return newPossibleFuncType()
	}},
	{"histget", func() vimType {
		return newPossibleFuncType()
	}},
	{"histnr", func() vimType {
		return newPossibleFuncType()
	}},
	{"hlexists", func() vimType {
		return newPossibleFuncType()
	}},
	{"hlID", func() vimType {
		return newPossibleFuncType()
	}},
	{"hostname", func() vimType {
		return newPossibleFuncType()
	}},
	{"iconv", func() vimType {
		return newPossibleFuncType()
	}},
	{"indent", func() vimType {
		return newPossibleFuncType()
	}},
	{"index", func() vimType {
		return newPossibleFuncType()
	}},
	{"input", func() vimType {
		return newPossibleFuncType()
	}},
	{"inputdialog", func() vimType {
		return newPossibleFuncType()
	}},
	{"inputlist", func() vimType {
		return newPossibleFuncType()
	}},
	{"inputrestore", func() vimType {
		return newPossibleFuncType()
	}},
	{"inputsave", func() vimType {
		return newPossibleFuncType()
	}},
	{"inputsecret", func() vimType {
		return newPossibleFuncType()
	}},
	{"insert", func() vimType {
		return newPossibleFuncType()
	}},
	{"invert", func() vimType {
		return newPossibleFuncType()
	}},
	{"isdirectory", func() vimType {
		return newPossibleFuncType()
	}},
	{"islocked", func() vimType {
		return newPossibleFuncType()
	}},
	{"isnan", func() vimType {
		return newPossibleFuncType()
	}},
	{"items", func() vimType {
		return newPossibleFuncType()
	}},
	{"job_getchannel", func() vimType {
		return newPossibleFuncType()
	}},
	{"job_info", func() vimType {
		return newPossibleFuncType()
	}},
	{"job_setoptions", func() vimType {
		return newPossibleFuncType()
	}},
	{"job_start", func() vimType {
		return newPossibleFuncType()
	}},
	{"job_status", func() vimType {
		return newPossibleFuncType()
	}},
	{"job_stop", func() vimType {
		return newPossibleFuncType()
	}},
	{"join", func() vimType {
		return newPossibleFuncType()
	}},
	{"js_decode", func() vimType {
		return newPossibleFuncType()
	}},
	{"js_encode", func() vimType {
		return newPossibleFuncType()
	}},
	{"json_decode", func() vimType {
		return newPossibleFuncType()
	}},
	{"json_encode", func() vimType {
		return newPossibleFuncType()
	}},
	{"keys", func() vimType {
		return newPossibleFuncType()
	}},
	{"len", func() vimType {
		return newPossibleFuncType()
	}},
	{"libcall", func() vimType {
		return newPossibleFuncType()
	}},
	{"libcallnr", func() vimType {
		return newPossibleFuncType()
	}},
	{"line", func() vimType {
		return newPossibleFuncType()
	}},
	{"line2byte", func() vimType {
		return newPossibleFuncType()
	}},
	{"lispindent", func() vimType {
		return newPossibleFuncType()
	}},
	{"localtime", func() vimType {
		return newPossibleFuncType()
	}},
	{"log", func() vimType {
		return newPossibleFuncType()
	}},
	{"log10", func() vimType {
		return newPossibleFuncType()
	}},
	{"luaeval", func() vimType {
		return newPossibleFuncType()
	}},
	{"map", func() vimType {
		return newPossibleFuncType()
	}},
	{"maparg", func() vimType {
		return newPossibleFuncType()
	}},
	{"mapcheck", func() vimType {
		return newPossibleFuncType()
	}},
	{"match", func() vimType {
		return newPossibleFuncType()
	}},
	{"matchadd", func() vimType {
		return newPossibleFuncType()
	}},
	{"matchaddpos", func() vimType {
		return newPossibleFuncType()
	}},
	{"matcharg", func() vimType {
		return newPossibleFuncType()
	}},
	{"matchdelete", func() vimType {
		return newPossibleFuncType()
	}},
	{"matchend", func() vimType {
		return newPossibleFuncType()
	}},
	{"matchlist", func() vimType {
		return newPossibleFuncType()
	}},
	{"matchstr", func() vimType {
		return newPossibleFuncType()
	}},
	{"matchstrpos", func() vimType {
		return newPossibleFuncType()
	}},
	{"max", func() vimType {
		return newPossibleFuncType()
	}},
	{"min", func() vimType {
		return newPossibleFuncType()
	}},
	{"mkdir", func() vimType {
		return newPossibleFuncType()
	}},
	{"mode", func() vimType {
		return newPossibleFuncType()
	}},
	{"mzeval", func() vimType {
		return newPossibleFuncType()
	}},
	{"nextnonblank", func() vimType {
		return newPossibleFuncType()
	}},
	{"nr2char", func() vimType {
		return newPossibleFuncType()
	}},
	{"or", func() vimType {
		return newFuncType(newTupleType(typeInt), typeInt)
	}},
	{"pathshorten", func() vimType {
		return newPossibleFuncType()
	}},
	{"perleval", func() vimType {
		return newPossibleFuncType()
	}},
	{"pow", func() vimType {
		return newPossibleFuncType()
	}},
	{"prevnonblank", func() vimType {
		return newPossibleFuncType()
	}},
	{"printf", func() vimType {
		return newPossibleFuncType()
	}},
	{"prompt_addtext", func() vimType {
		return newPossibleFuncType()
	}},
	{"prompt_setcallback", func() vimType {
		return newPossibleFuncType()
	}},
	{"prompt_setinterrupt", func() vimType {
		return newPossibleFuncType()
	}},
	{"prompt_setprompt", func() vimType {
		return newPossibleFuncType()
	}},
	{"pumvisible", func() vimType {
		return newPossibleFuncType()
	}},
	{"pyeval", func() vimType {
		return newPossibleFuncType()
	}},
	{"py3eval", func() vimType {
		return newPossibleFuncType()
	}},
	{"pyxeval", func() vimType {
		return newPossibleFuncType()
	}},
	{"range", func() vimType {
		return newPossibleFuncType()
	}},
	{"readfile", func() vimType {
		return newPossibleFuncType()
	}},
	{"reg_executing", func() vimType {
		return newPossibleFuncType()
	}},
	{"reg_recording", func() vimType {
		return newPossibleFuncType()
	}},
	{"reltime", func() vimType {
		return newPossibleFuncType()
	}},
	{"reltimefloat", func() vimType {
		return newPossibleFuncType()
	}},
	{"reltimestr", func() vimType {
		return newPossibleFuncType()
	}},
	{"remote_expr", func() vimType {
		return newPossibleFuncType()
	}},
	{"remote_foreground", func() vimType {
		return newPossibleFuncType()
	}},
	{"remote_peek", func() vimType {
		return newPossibleFuncType()
	}},
	{"remote_read", func() vimType {
		return newPossibleFuncType()
	}},
	{"remote_send", func() vimType {
		return newPossibleFuncType()
	}},
	{"remote_startserver", func() vimType {
		return newPossibleFuncType()
	}},
	{"remove", func() vimType {
		return newPossibleFuncType()
	}},
	{"rename", func() vimType {
		return newPossibleFuncType()
	}},
	{"repeat", func() vimType {
		return newPossibleFuncType()
	}},
	{"resolve", func() vimType {
		return newPossibleFuncType()
	}},
	{"reverse", func() vimType {
		return newPossibleFuncType()
	}},
	{"round", func() vimType {
		return newPossibleFuncType()
	}},
	{"screenattr", func() vimType {
		return newPossibleFuncType()
	}},
	{"screenchar", func() vimType {
		return newPossibleFuncType()
	}},
	{"screencol", func() vimType {
		return newPossibleFuncType()
	}},
	{"screenrow", func() vimType {
		return newPossibleFuncType()
	}},
	{"search", func() vimType {
		return newPossibleFuncType()
	}},
	{"searchdecl", func() vimType {
		return newPossibleFuncType()
	}},
	{"searchpair", func() vimType {
		return newPossibleFuncType()
	}},
	{"searchpairpos", func() vimType {
		return newPossibleFuncType()
	}},
	{"searchpos", func() vimType {
		return newPossibleFuncType()
	}},
	{"server2client", func() vimType {
		return newPossibleFuncType()
	}},
	{"serverlist", func() vimType {
		return newPossibleFuncType()
	}},
	{"setbufline", func() vimType {
		return newPossibleFuncType()
	}},
	{"setbufvar", func() vimType {
		return newPossibleFuncType()
	}},
	{"setcharsearch", func() vimType {
		return newPossibleFuncType()
	}},
	{"setcmdpos", func() vimType {
		return newPossibleFuncType()
	}},
	{"setfperm", func() vimType {
		return newPossibleFuncType()
	}},
	{"setline", func() vimType {
		return newPossibleFuncType()
	}},
	{"setloclist", func() vimType {
		return newPossibleFuncType()
	}},
	{"setmatches", func() vimType {
		return newPossibleFuncType()
	}},
	{"setpos", func() vimType {
		return newPossibleFuncType()
	}},
	{"setqflist", func() vimType {
		return newPossibleFuncType()
	}},
	{"setreg", func() vimType {
		return newPossibleFuncType()
	}},
	{"settabvar", func() vimType {
		return newPossibleFuncType()
	}},
	{"settabwinvar", func() vimType {
		return newPossibleFuncType()
	}},
	{"setwinvar", func() vimType {
		return newPossibleFuncType()
	}},
	{"sha256", func() vimType {
		return newPossibleFuncType()
	}},
	{"shellescape", func() vimType {
		return newPossibleFuncType()
	}},
	{"shiftwidth", func() vimType {
		return newPossibleFuncType()
	}},
	{"simplify", func() vimType {
		return newPossibleFuncType()
	}},
	{"sin", func() vimType {
		return newPossibleFuncType()
	}},
	{"sinh", func() vimType {
		return newPossibleFuncType()
	}},
	{"sort", func() vimType {
		return newPossibleFuncType()
	}},
	{"soundfold", func() vimType {
		return newPossibleFuncType()
	}},
	{"spellbadword", func() vimType {
		return newPossibleFuncType()
	}},
	{"spellsuggest", func() vimType {
		return newPossibleFuncType()
	}},
	{"split", func() vimType {
		return newPossibleFuncType()
	}},
	{"sqrt", func() vimType {
		return newPossibleFuncType()
	}},
	{"str2float", func() vimType {
		return newPossibleFuncType()
	}},
	{"str2nr", func() vimType {
		return newPossibleFuncType()
	}},
	{"strchars", func() vimType {
		return newPossibleFuncType()
	}},
	{"strcharpart", func() vimType {
		return newPossibleFuncType()
	}},
	{"strdisplaywidth", func() vimType {
		return newPossibleFuncType()
	}},
	{"strftime", func() vimType {
		return newPossibleFuncType()
	}},
	{"strgetchar", func() vimType {
		return newPossibleFuncType()
	}},
	{"stridx", func() vimType {
		return newPossibleFuncType()
	}},
	{"string", func() vimType {
		return newPossibleFuncType()
	}},
	{"strlen", func() vimType {
		return newPossibleFuncType()
	}},
	{"strpart", func() vimType {
		return newPossibleFuncType()
	}},
	{"strridx", func() vimType {
		return newPossibleFuncType()
	}},
	{"strtrans", func() vimType {
		return newPossibleFuncType()
	}},
	{"strwidth", func() vimType {
		return newPossibleFuncType()
	}},
	{"submatch", func() vimType {
		return newPossibleFuncType()
	}},
	{"substitute", func() vimType {
		return newPossibleFuncType()
	}},
	{"swapinfo", func() vimType {
		return newPossibleFuncType()
	}},
	{"synID", func() vimType {
		return newPossibleFuncType()
	}},
	{"synIDattr", func() vimType {
		return newPossibleFuncType()
	}},
	{"synIDtrans", func() vimType {
		return newPossibleFuncType()
	}},
	{"synconcealed", func() vimType {
		return newPossibleFuncType()
	}},
	{"synstack", func() vimType {
		return newPossibleFuncType()
	}},
	{"system", func() vimType {
		return newPossibleFuncType()
	}},
	{"systemlist", func() vimType {
		return newPossibleFuncType()
	}},
	{"tabpagebuflist", func() vimType {
		return newPossibleFuncType()
	}},
	{"tabpagenr", func() vimType {
		return newPossibleFuncType()
	}},
	{"tabpagewinnr", func() vimType {
		return newPossibleFuncType()
	}},
	{"taglist", func() vimType {
		return newPossibleFuncType()
	}},
	{"tagfiles", func() vimType {
		return newPossibleFuncType()
	}},
	{"tan", func() vimType {
		return newPossibleFuncType()
	}},
	{"tanh", func() vimType {
		return newPossibleFuncType()
	}},
	{"tempname", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_dumpdiff", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_dumpload", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_dumpwrite", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_getaltscreen", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_getansicolors", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_getattr", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_getcursor", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_getjob", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_getline", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_getscrolled", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_getsize", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_getstatus", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_gettitle", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_gettty", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_list", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_scrape", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_sendkeys", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_setansicolors", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_setkill", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_setrestore", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_setsize", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_start", func() vimType {
		return newPossibleFuncType()
	}},
	{"term_wait", func() vimType {
		return newPossibleFuncType()
	}},
	{"test_alloc_fail", func() vimType {
		return newPossibleFuncType()
	}},
	{"test_autochdir", func() vimType {
		return newPossibleFuncType()
	}},
	{"test_feedinput", func() vimType {
		return newPossibleFuncType()
	}},
	{"test_garbagecollect_now", func() vimType {
		return newPossibleFuncType()
	}},
	{"test_ignore_error", func() vimType {
		return newPossibleFuncType()
	}},
	{"test_null_channel", func() vimType {
		return newPossibleFuncType()
	}},
	{"test_null_dict", func() vimType {
		return newPossibleFuncType()
	}},
	{"test_null_job", func() vimType {
		return newPossibleFuncType()
	}},
	{"test_null_list", func() vimType {
		return newPossibleFuncType()
	}},
	{"test_null_partial", func() vimType {
		return newPossibleFuncType()
	}},
	{"test_null_string", func() vimType {
		return newPossibleFuncType()
	}},
	{"test_override", func() vimType {
		return newPossibleFuncType()
	}},
	{"test_settime", func() vimType {
		return newPossibleFuncType()
	}},
	{"timer_info", func() vimType {
		return newPossibleFuncType()
	}},
	{"timer_pause", func() vimType {
		return newPossibleFuncType()
	}},
	{"timer_start", func() vimType {
		return newPossibleFuncType()
	}},
	{"timer_stop", func() vimType {
		return newPossibleFuncType()
	}},
	{"timer_stopall", func() vimType {
		return newPossibleFuncType()
	}},
	{"tolower", func() vimType {
		return newPossibleFuncType()
	}},
	{"toupper", func() vimType {
		return newPossibleFuncType()
	}},
	{"tr", func() vimType {
		return newPossibleFuncType()
	}},
	{"trim", func() vimType {
		return newPossibleFuncType()
	}},
	{"trunc", func() vimType {
		return newPossibleFuncType()
	}},
	{"type", func() vimType {
		return newPossibleFuncType()
	}},
	{"undofile", func() vimType {
		return newPossibleFuncType()
	}},
	{"undotree", func() vimType {
		return newPossibleFuncType()
	}},
	{"uniq", func() vimType {
		return newPossibleFuncType()
	}},
	{"values", func() vimType {
		return newPossibleFuncType()
	}},
	{"virtcol", func() vimType {
		return newPossibleFuncType()
	}},
	{"visualmode", func() vimType {
		return newPossibleFuncType()
	}},
	{"wildmenumode", func() vimType {
		return newPossibleFuncType()
	}},
	{"win_findbuf", func() vimType {
		return newPossibleFuncType()
	}},
	{"win_getid", func() vimType {
		return newPossibleFuncType()
	}},
	{"win_gotoid", func() vimType {
		return newPossibleFuncType()
	}},
	{"win_id2tabwin", func() vimType {
		return newPossibleFuncType()
	}},
	{"win_id2win", func() vimType {
		return newPossibleFuncType()
	}},
	{"win_screenpos", func() vimType {
		return newPossibleFuncType()
	}},
	{"winbufnr", func() vimType {
		return newPossibleFuncType()
	}},
	{"wincol", func() vimType {
		return newPossibleFuncType()
	}},
	{"winheight", func() vimType {
		return newPossibleFuncType()
	}},
	{"winlayout", func() vimType {
		return newPossibleFuncType()
	}},
	{"winline", func() vimType {
		return newPossibleFuncType()
	}},
	{"winnr", func() vimType {
		return newPossibleFuncType()
	}},
	{"winrestcmd", func() vimType {
		return newPossibleFuncType()
	}},
	{"winrestview", func() vimType {
		return newPossibleFuncType()
	}},
	{"winsaveview", func() vimType {
		return newPossibleFuncType()
	}},
	{"winwidth", func() vimType {
		return newPossibleFuncType()
	}},
	{"wordcount", func() vimType {
		return newPossibleFuncType()
	}},
	{"writefile", func() vimType {
		return newPossibleFuncType()
	}},
	{"xor", func() vimType {
		return newFuncType(newTupleType(typeInt), typeInt)
	}},
}

var vimOptions = []vimOption{
	{"aleph", 2, typeInt},
	{"allowrevins", 3, typeBoolean},
	{"altkeymap", 3, typeBoolean},
	{"ambiwidth", 4, typeString},
	{"antialias", 4, typeBoolean},
	{"autochdir", 3, typeBoolean},
	{"arabic", 4, typeBoolean},
	{"arabicshape", 7, typeBoolean},
	{"autoindent", 2, typeBoolean},
	{"autoread", 2, typeBoolean},
	{"autowrite", 2, typeBoolean},
	{"autowriteall", 3, typeBoolean},
	{"background", 2, typeString},
	{"backspace", 2, typeString},
	{"backup", 2, typeBoolean},
	{"backupcopy", 3, typeString},
	{"backupdir", 4, typeString},
	{"backupext", 3, typeString},
	{"backupskip", 3, typeString},
	{"balloondelay", 5, typeInt},
	{"ballooneval", 5, typeBoolean},
	{"balloonevalterm", 9, typeBoolean},
	{"balloonexpr", 5, typeString},
	{"belloff", 2, typeString},
	{"binary", 3, typeBoolean},
	{"bioskey", 5, typeBoolean},
	{"bomb", 4, typeBoolean},
	{"breakat", 3, typeString},
	{"breakindent", 3, typeBoolean},
	{"breakindentopt", 6, typeString},
	{"browsedir", 5, typeString},
	{"bufhidden", 2, typeString},
	{"buflisted", 2, typeBoolean},
	{"buftype", 2, typeString},
	{"casemap", 3, typeString},
	{"cdpath", 2, typeString},
	{"cedit", 5, typeString},
	{"charconvert", 3, typeString},
	{"cindent", 3, typeBoolean},
	{"cinkeys", 4, typeString},
	{"cinoptions", 4, typeString},
	{"cinwords", 4, typeString},
	{"clipboard", 2, typeString},
	{"cmdheight", 2, typeInt},
	{"cmdwinheight", 3, typeInt},
	{"colorcolumn", 2, typeString},
	{"columns", 2, typeInt},
	{"comments", 3, typeString},
	{"commentstring", 3, typeString},
	{"compatible", 2, typeBoolean},
	{"complete", 3, typeString},
	{"completefunc", 3, typeString},
	{"completeopt", 3, typeString},
	{"concealcursor", 4, typeString},
	{"conceallevel", 4, typeInt},
	{"confirm", 2, typeBoolean},
	{"conskey", 5, typeBoolean},
	{"copyindent", 2, typeBoolean},
	{"cpoptions", 3, typeString},
	{"cryptmethod", 2, typeString},
	{"cscopepathcomp", 4, typeInt},
	{"cscopeprg", 5, typeString},
	{"cscopequickfix", 4, typeString},
	{"cscoperelative", 4, typeBoolean},
	{"cscopetag", 3, typeBoolean},
	{"cscopetagorder", 4, typeInt},
	{"cscopeverbose", 6, typeBoolean},
	{"cursorbind", 3, typeBoolean},
	{"cursorcolumn", 3, typeBoolean},
	{"cursorline", 3, typeBoolean},
	{"debug", 5, typeString},
	{"define", 3, typeString},
	{"delcombine", 4, typeBoolean},
	{"dictionary", 4, typeString},
	{"diff", 4, typeBoolean},
	{"diffexpr", 3, typeString},
	{"diffopt", 3, typeString},
	{"digraph", 2, typeBoolean},
	{"directory", 3, typeString},
	{"display", 2, typeString},
	{"eadirection", 3, typeString},
	{"edcompatible", 2, typeBoolean},
	{"emoji", 3, typeBoolean},
	{"encoding", 3, typeString},
	{"endofline", 3, typeBoolean},
	{"equalalways", 2, typeBoolean},
	{"equalprg", 2, typeString},
	{"errorbells", 2, typeBoolean},
	{"errorfile", 2, typeString},
	{"errorformat", 3, typeString},
	{"esckeys", 2, typeBoolean},
	{"eventignore", 2, typeString},
	{"expandtab", 2, typeBoolean},
	{"exrc", 2, typeBoolean},
	{"fileencoding", 4, typeString},
	{"fileencodings", 5, typeString},
	{"fileformat", 2, typeString},
	{"fileformats", 3, typeString},
	{"fileignorecase", 3, typeBoolean},
	{"filetype", 2, typeString},
	{"fillchars", 3, typeString},
	{"fixendofline", 6, typeBoolean},
	{"fkmap", 2, typeBoolean},
	{"foldclose", 3, typeString},
	{"foldcolumn", 3, typeInt},
	{"foldenable", 3, typeBoolean},
	{"foldexpr", 3, typeString},
	{"foldignore", 3, typeString},
	{"foldlevel", 3, typeInt},
	{"foldlevelstart", 4, typeInt},
	{"foldmarker", 3, typeString},
	{"foldmethod", 3, typeString},
	{"foldminlines", 3, typeInt},
	{"foldnestmax", 3, typeInt},
	{"foldopen", 3, typeString},
	{"foldtext", 3, typeString},
	{"formatexpr", 3, typeString},
	{"formatoptions", 2, typeString},
	{"formatlistpat", 3, typeString},
	{"formatprg", 2, typeString},
	{"fsync", 2, typeBoolean},
	{"gdefault", 2, typeBoolean},
	{"grepformat", 3, typeString},
	{"grepprg", 2, typeString},
	{"guicursor", 3, typeString},
	{"guifont", 3, typeString},
	{"guifontset", 3, typeString},
	{"guifontwide", 3, typeString},
	{"guiheadroom", 3, typeInt},
	{"guioptions", 2, typeString},
	{"guipty", 6, typeBoolean},
	{"guitablabel", 3, typeString},
	{"guitabtooltip", 3, typeString},
	{"helpfile", 2, typeString},
	{"helpheight", 2, typeInt},
	{"helplang", 3, typeString},
	{"hidden", 3, typeBoolean},
	{"highlight", 2, typeString},
	{"history", 2, typeInt},
	{"hkmap", 2, typeBoolean},
	{"hkmapp", 3, typeBoolean},
	{"hlsearch", 3, typeBoolean},
	{"icon", 4, typeBoolean},
	{"iconstring", 10, typeString},
	{"ignorecase", 2, typeBoolean},
	{"imactivatefunc", 4, typeString},
	{"imactivatekey", 4, typeString},
	{"imcmdline", 3, typeBoolean},
	{"imdisable", 3, typeBoolean},
	{"iminsert", 3, typeInt},
	{"imsearch", 3, typeInt},
	{"imstatusfunc", 4, typeString},
	{"imstyle", 4, typeInt},
	{"include", 3, typeString},
	{"includeexpr", 4, typeString},
	{"incsearch", 2, typeBoolean},
	{"indentexpr", 4, typeString},
	{"indentkeys", 4, typeString},
	{"infercase", 3, typeBoolean},
	{"insertmode", 2, typeBoolean},
	{"isfname", 3, typeString},
	{"isident", 3, typeString},
	{"iskeyword", 3, typeString},
	{"isprint", 3, typeString},
	{"joinspaces", 2, typeBoolean},
	{"key", 3, typeString},
	{"keymap", 3, typeString},
	{"keymodel", 2, typeString},
	{"keywordprg", 2, typeString},
	{"langmap", 4, typeString},
	{"langmenu", 2, typeString},
	{"langnoremap", 3, typeBoolean},
	{"langremap", 3, typeBoolean},
	{"laststatus", 2, typeInt},
	{"lazyredraw", 2, typeBoolean},
	{"linebreak", 3, typeBoolean},
	{"lines", 5, typeInt},
	{"linespace", 3, typeInt},
	{"lisp", 4, typeBoolean},
	{"lispwords", 2, typeString},
	{"list", 4, typeBoolean},
	{"listchars", 3, typeString},
	{"loadplugins", 3, typeBoolean},
	{"luadll", 6, typeString},
	{"macatsui", 8, typeBoolean},
	{"magic", 5, typeBoolean},
	{"makeef", 3, typeString},
	{"makeencoding", 4, typeString},
	{"makeprg", 2, typeString},
	{"matchpairs", 3, typeString},
	{"matchtime", 3, typeInt},
	{"maxcombine", 3, typeInt},
	{"maxfuncdepth", 3, typeInt},
	{"maxmapdepth", 3, typeInt},
	{"maxmem", 2, typeInt},
	{"maxmempattern", 3, typeInt},
	{"maxmemtot", 3, typeInt},
	{"menuitems", 3, typeInt},
	{"mkspellmem", 3, typeString},
	{"modeline", 2, typeBoolean},
	{"modelines", 3, typeInt},
	{"modifiable", 2, typeBoolean},
	{"modified", 3, typeBoolean},
	{"more", 4, typeBoolean},
	{"mouse", 5, typeString},
	{"mousefocus", 6, typeBoolean},
	{"mousehide", 2, typeBoolean},
	{"mousemodel", 6, typeString},
	{"mouseshape", 6, typeString},
	{"mousetime", 6, typeInt},
	{"mzschemedll", 11, typeString},
	{"mzschemegcdll", 13, typeString},
	{"mzquantum", 3, typeInt},
	{"nrformats", 2, typeString},
	{"number", 2, typeBoolean},
	{"numberwidth", 3, typeInt},
	{"omnifunc", 3, typeString},
	{"opendevice", 4, typeBoolean},
	{"operatorfunc", 6, typeString},
	{"osfiletype", 3, typeString},
	{"packpath", 2, typeString},
	{"paragraphs", 4, typeString},
	{"paste", 5, typeBoolean},
	{"pastetoggle", 2, typeString},
	{"patchexpr", 3, typeString},
	{"patchmode", 2, typeString},
	{"path", 2, typeString},
	{"perldll", 7, typeString},
	{"preserveindent", 2, typeBoolean},
	{"previewheight", 3, typeInt},
	{"previewwindow", 3, typeBoolean},
	{"printdevice", 4, typeString},
	{"printencoding", 4, typeString},
	{"printexpr", 5, typeString},
	{"printfont", 3, typeString},
	{"printheader", 7, typeString},
	{"printmbcharset", 5, typeString},
	{"printmbfont", 5, typeString},
	{"printoptions", 4, typeString},
	{"prompt", 6, typeBoolean},
	{"pumheight", 2, typeInt},
	{"pumwidth", 2, typeInt},
	{"pythondll", 9, typeString},
	{"pythonhome", 10, typeString},
	{"pythonthreedll", 14, typeString},
	{"pythonthreehome", 15, typeString},
	{"pyxversion", 3, typeInt},
	{"quoteescape", 2, typeString},
	{"readonly", 2, typeBoolean},
	{"redrawtime", 3, typeInt},
	{"regexpengine", 2, typeInt},
	{"relativenumber", 3, typeBoolean},
	{"remap", 5, typeBoolean},
	{"renderoptions", 3, typeString},
	{"report", 6, typeInt},
	{"restorescreen", 2, typeBoolean},
	{"revins", 2, typeBoolean},
	{"rightleft", 2, typeBoolean},
	{"rightleftcmd", 3, typeString},
	{"rubydll", 7, typeString},
	{"ruler", 2, typeBoolean},
	{"rulerformat", 3, typeString},
	{"runtimepath", 3, typeString},
	{"scroll", 3, typeInt},
	{"scrollbind", 3, typeBoolean},
	{"scrolljump", 2, typeInt},
	{"scrolloff", 2, typeInt},
	{"scrollopt", 3, typeString},
	{"sections", 4, typeString},
	{"secure", 6, typeBoolean},
	{"selection", 3, typeString},
	{"selectmode", 3, typeString},
	{"sessionoptions", 4, typeString},
	{"shell", 2, typeString},
	{"shellcmdflag", 4, typeString},
	{"shellpipe", 2, typeString},
	{"shellquote", 3, typeString},
	{"shellredir", 3, typeString},
	{"shellslash", 3, typeBoolean},
	{"shelltemp", 4, typeBoolean},
	{"shelltype", 2, typeInt},
	{"shellxescape", 3, typeString},
	{"shellxquote", 3, typeString},
	{"shiftround", 2, typeBoolean},
	{"shiftwidth", 2, typeInt},
	{"shortmess", 3, typeString},
	{"shortname", 2, typeBoolean},
	{"showbreak", 3, typeString},
	{"showcmd", 2, typeBoolean},
	{"showfulltag", 3, typeBoolean},
	{"showmatch", 2, typeBoolean},
	{"showmode", 3, typeBoolean},
	{"showtabline", 4, typeInt},
	{"sidescroll", 2, typeInt},
	{"sidescrolloff", 4, typeInt},
	{"signcolumn", 3, typeString},
	{"smartcase", 3, typeBoolean},
	{"smartindent", 2, typeBoolean},
	{"smarttab", 3, typeBoolean},
	{"softtabstop", 3, typeInt},
	{"spell", 5, typeBoolean},
	{"spellcapcheck", 3, typeString},
	{"spellfile", 3, typeString},
	{"spelllang", 3, typeString},
	{"spellsuggest", 3, typeString},
	{"splitbelow", 2, typeBoolean},
	{"splitright", 3, typeBoolean},
	{"startofline", 3, typeBoolean},
	{"statusline", 3, typeString},
	{"suffixes", 2, typeString},
	{"suffixesadd", 3, typeString},
	{"swapfile", 3, typeBoolean},
	{"swapsync", 3, typeString},
	{"switchbuf", 3, typeString},
	{"synmaxcol", 3, typeInt},
	{"syntax", 3, typeString},
	{"tabline", 3, typeString},
	{"tabpagemax", 3, typeInt},
	{"tabstop", 2, typeInt},
	{"tagbsearch", 3, typeBoolean},
	{"tagcase", 2, typeString},
	{"taglength", 2, typeInt},
	{"tagrelative", 2, typeBoolean},
	{"tags", 3, typeString},
	{"tagstack", 4, typeBoolean},
	{"tcldll", 6, typeString},
	{"term", 4, typeString},
	{"termbidi", 5, typeBoolean},
	{"termencoding", 4, typeString},
	{"termguicolors", 3, typeBoolean},
	{"termwinscroll", 4, typeInt},
	{"termwinkey", 3, typeString},
	{"termwinsize", 3, typeString},
	{"terse", 5, typeBoolean},
	{"textauto", 2, typeBoolean},
	{"textmode", 2, typeBoolean},
	{"textwidth", 2, typeInt},
	{"thesaurus", 3, typeString},
	{"tildeop", 3, typeBoolean},
	{"timeout", 2, typeBoolean},
	{"ttimeout", 8, typeBoolean},
	{"timeoutlen", 2, typeInt},
	{"ttimeoutlen", 3, typeInt},
	{"title", 5, typeBoolean},
	{"titlelen", 8, typeInt},
	{"titleold", 8, typeString},
	{"titlestring", 11, typeString},
	{"toolbar", 2, typeString},
	{"toolbariconsize", 4, typeString},
	{"ttybuiltin", 3, typeBoolean},
	{"ttyfast", 2, typeBoolean},
	{"ttymouse", 4, typeString},
	{"ttyscroll", 3, typeInt},
	{"ttytype", 3, typeString},
	{"undodir", 4, typeString},
	{"undofile", 3, typeBoolean},
	{"undolevels", 2, typeInt},
	{"undoreload", 2, typeInt},
	{"updatecount", 2, typeInt},
	{"updatetime", 2, typeInt},
	{"varsofttabstop", 4, typeString},
	{"vartabstop", 3, typeString},
	{"verbose", 3, typeInt},
	{"verbosefile", 5, typeString},
	{"viewdir", 4, typeString},
	{"viewoptions", 3, typeString},
	{"viminfo", 2, typeString},
	{"viminfofile", 3, typeString},
	{"virtualedit", 2, typeString},
	{"visualbell", 2, typeBoolean},
	{"warn", 4, typeBoolean},
	{"weirdinvert", 3, typeBoolean},
	{"whichwrap", 2, typeString},
	{"wildchar", 2, typeInt},
	{"wildcharm", 3, typeInt},
	{"wildignore", 3, typeString},
	{"wildignorecase", 3, typeBoolean},
	{"wildmenu", 4, typeBoolean},
	{"wildmode", 3, typeString},
	{"wildoptions", 3, typeString},
	{"winaltkeys", 3, typeString},
	{"window", 2, typeInt},
	{"winheight", 2, typeInt},
	{"winfixheight", 3, typeBoolean},
	{"winfixwidth", 3, typeBoolean},
	{"winminheight", 3, typeInt},
	{"winminwidth", 3, typeInt},
	{"winptydll", 9, typeString},
	{"winwidth", 3, typeInt},
	{"wrap", 4, typeBoolean},
	{"wrapmargin", 2, typeInt},
	{"wrapscan", 2, typeBoolean},
	{"write", 5, typeBoolean},
	{"writeany", 2, typeBoolean},
	{"writebackup", 2, typeBoolean},
	{"writedelay", 2, typeInt},
}

var vimVariables = []vimVar{
	{"a", "", vvRO, funcOf(typeDict)},
	{"a", "0", vvRO, funcOf(typeInt)},
	{"a", "000", vvRO, funcOf(typeList)},
	{"a", "firstline", vvRO, funcOf(typeInt)},
	{"a", "lastline", vvRO, funcOf(typeInt)},
	{"b", "", vvRO, funcOf(typeDict)},
	{"b", "changedtick", vvCompat + vvRO, funcOf(typeInt)},
	{"g", "", vvRO, funcOf(typeDict)},
	{"l", "", vvRO, funcOf(typeDict)},
	{"s", "", vvRO, funcOf(typeDict)},
	{"t", "", vvRO, funcOf(typeDict)},
	{"v", "", vvRO, funcOf(typeDict)},
	{"v", "beval_bufnr", vvRO, funcOf(typeInt)},
	{"v", "beval_col", vvRO, funcOf(typeInt)},
	{"v", "beval_lnum", vvRO, funcOf(typeInt)},
	{"v", "beval_text", vvRO, funcOf(typeString)},
	{"v", "beval_winid", vvRO, funcOf(typeInt)},
	{"v", "beval_winnr", vvRO, funcOf(typeInt)},
	{"v", "char", 0, funcOf(typeString)},
	{"v", "charconvert_from", vvRO, funcOf(typeString)},
	{"v", "charconvert_to", vvRO, funcOf(typeString)},
	{"v", "cmdarg", vvRO, funcOf(typeString)},
	{"v", "cmdbang", vvRO, funcOf(typeInt)},
	{"v", "completed_item", vvRO, funcOf(typeDict)},
	{"v", "count", vvCompat + vvRO, funcOf(typeInt)},
	{"v", "count1", vvRO, funcOf(typeInt)},
	{"v", "ctype", vvRO, funcOf(typeString)},
	{"v", "dying", vvRO, funcOf(typeInt)},
	{"v", "errmsg", vvCompat, funcOf(typeString)},
	{"v", "errors", 0, funcOf(typeList)},
	{"v", "event", vvRO, funcOf(typeDict)},
	{"v", "exception", vvRO, funcOf(typeString)},
	{"v", "false", vvRO, funcOf(typeBoolean)},
	{"v", "fcs_choice", 0, funcOf(typeString)},
	{"v", "fcs_reason", vvRO, funcOf(typeString)},
	{"v", "fname", vvRO, funcOf(typeString)},
	{"v", "fname_diff", vvRO, funcOf(typeString)},
	{"v", "fname_in", vvRO, funcOf(typeString)},
	{"v", "fname_new", vvRO, funcOf(typeString)},
	{"v", "fname_out", vvRO, funcOf(typeString)},
	{"v", "folddashes", vvROSBX, funcOf(typeString)},
	{"v", "foldend", vvROSBX, funcOf(typeInt)},
	{"v", "foldlevel", vvROSBX, funcOf(typeInt)},
	{"v", "foldstart", vvROSBX, funcOf(typeInt)},
	{"v", "hlsearch", 0, funcOf(typeInt)},
	{"v", "insertmode", vvRO, funcOf(typeString)},
	{"v", "key", vvRO, func() vimType { return newTypeVar() }},
	{"v", "lang", vvRO, funcOf(typeString)},
	{"v", "lc_time", vvRO, funcOf(typeString)},
	{"v", "lnum", vvROSBX, funcOf(typeInt)},
	{"v", "mouse_col", 0, funcOf(typeInt)},
	{"v", "mouse_lnum", 0, funcOf(typeInt)},
	{"v", "mouse_win", 0, funcOf(typeInt)},
	{"v", "mouse_winid", 0, funcOf(typeInt)},
	{"v", "none", vvRO, funcOf(typeNone)},
	{"v", "null", vvRO, funcOf(typeNone)},
	{"v", "oldfiles", 0, funcOf(typeList)},
	{"v", "operator", vvRO, funcOf(typeString)},
	{"v", "option_new", vvRO, funcOf(typeString)},
	{"v", "option_old", vvRO, funcOf(typeString)},
	{"v", "option_type", vvRO, funcOf(typeString)},
	{"v", "prevcount", vvRO, funcOf(typeInt)},
	{"v", "profiling", vvRO, funcOf(typeInt)},
	{"v", "progname", vvRO, funcOf(typeString)},
	{"v", "progpath", vvRO, funcOf(typeString)},
	{"v", "register", vvRO, funcOf(typeString)},
	{"v", "scrollstart", 0, funcOf(typeString)},
	{"v", "searchforward", 0, funcOf(typeInt)},
	{"v", "servername", vvRO, funcOf(typeString)},
	{"v", "shell_error", vvCompat + vvRO, funcOf(typeInt)},
	{"v", "statusmsg", 0, funcOf(typeString)},
	{"v", "swapchoice", 0, funcOf(typeString)},
	{"v", "swapcommand", vvRO, funcOf(typeString)},
	{"v", "swapname", vvRO, funcOf(typeString)},
	{"v", "t_bool", vvRO, funcOf(typeInt)},
	{"v", "t_channel", vvRO, funcOf(typeInt)},
	{"v", "t_dict", vvRO, funcOf(typeInt)},
	{"v", "t_float", vvRO, funcOf(typeInt)},
	{"v", "t_func", vvRO, funcOf(typeInt)},
	{"v", "t_job", vvRO, funcOf(typeInt)},
	{"v", "t_list", vvRO, funcOf(typeInt)},
	{"v", "t_none", vvRO, funcOf(typeInt)},
	{"v", "t_number", vvRO, funcOf(typeInt)},
	{"v", "t_string", vvRO, funcOf(typeInt)},
	{"v", "termblinkresp", vvRO, funcOf(typeString)},
	{"v", "termrbgresp", vvRO, funcOf(typeString)},
	{"v", "termresponse", vvRO, funcOf(typeString)},
	{"v", "termrfgresp", vvRO, funcOf(typeString)},
	{"v", "termstyleresp", vvRO, funcOf(typeString)},
	{"v", "termu7resp", vvRO, funcOf(typeString)},
	{"v", "testing", 0, funcOf(typeInt)},
	{"v", "this_session", vvCompat, funcOf(typeString)},
	{"v", "throwpoint", vvRO, funcOf(typeString)},
	{"v", "true", vvRO, funcOf(typeBoolean)},
	{"v", "val", vvRO, func() vimType { return newTypeVar() }},
	{"v", "version", vvCompat + vvRO, funcOf(typeInt)},
	{"v", "vim_did_enter", vvRO, funcOf(typeInt)},
	{"v", "warningmsg", 0, funcOf(typeString)},
	{"v", "windowid", vvRO, funcOf(typeInt)},
	{"w", "", vvRO, funcOf(typeDict)},
}

// ===================================================

func getTypeOfVimOption(name string) vimType {
	if v := lookUpOption(name); v != nil {
		return v.typ
	}
	return nil
}

func lookUpOption(name string) *vimOption {
	i := sort.Search(len(vimOptions), func(i int) bool {
		return strings.Compare(vimOptions[i].name, name) >= 0
	})
	if i >= len(vimOptions) {
		return nil
	}
	return &vimOptions[i]
}

type vimOption struct {
	name string
	min  int
	typ  vimType
}

func getVimVarCName(name string) *CName {
	scope, name := splitVimVar(name)
	if !isVimVar(scope, name) {
		return nil
	}
	return newCName(scope, name, "")
}

func getVimVarType(cname *CName) vimType {
	if v := lookUpVar(cname.scope, cname.varname); v != nil {
		return v.typ()
	}
	return nil
}

func isVimVar(scope, name string) bool {
	return lookUpVar(scope, name) != nil
}

func lookUpVar(scope, name string) *vimVar {
	if scope == "" { // scope == "g", name == "", cname == "g:"
		return nil
	}
	varname := scope + ":" + name
	i := sort.Search(len(vimVariables), func(i int) bool {
		n := vimVariables[i].scope + ":" + vimVariables[i].name
		return strings.Compare(n, varname) >= 0
	})
	if i >= len(vimVariables) {
		return nil
	}
	return &vimVariables[i]
}

// splitVimVar splits variable name into scope and variable name.
// If scope != "", varname has a scope, or dict variable like "g:".
// Note that some variables like "count" become "v:count" for compatibility reason.
//
//	splitVimVar("name") = "", "name"
//	splitVimVar("g:") = "g", ""
//	splitVimVar("g:name") = "g", "name"
//	splitVimVar("count") = "v", "count"
//	splitVimVar("g:count") = "g", "count"
func splitVimVar(varname string) (scope, name string) {
	if varname == "" {
		return
	}
	if len(varname) >= 2 &&
		strings.ContainsAny(varname[:1], "bwtglsav") &&
		varname[1] == ':' { // has a scope
		scope = varname[:1]
		name = varname[2:]
		return
	}
	if isCompatVimVar(varname) {
		scope = "v"
		name = varname
		return
	}
	name = varname
	return
}

func isCompatVimVar(name string) bool {
	switch name {
	case "count":
	default:
		return false
	}
	return true
}

type vimVar struct {
	scope string
	name  string
	flags vimVarFlag
	typ   func() vimType
}

type vimVarFlag int

const (
	vvCompat vimVarFlag = 1 // compatible, also used without "v:"
	vvRO     vimVarFlag = 2 // read-only
	vvROSBX  vimVarFlag = 4 // read-only in the sandbox
)

func funcOf(t typeID) func() vimType {
	return func() vimType { return t }
}

func getVimFuncCName(name string) *CName {
	if !isVimFunc(name) {
		return nil
	}
	return newCName("vimfunc", name, "")
}

func getVimFuncType(cname *CName) vimType {
	if fun := lookUpFunc(cname.varname); fun != nil {
		return fun.typ()
	}
	return nil
}

func isVimFunc(name string) bool {
	return lookUpFunc(name) != nil
}

func lookUpFunc(name string) *vimFunc {
	i := sort.Search(len(vimFunctions), func(i int) bool {
		return strings.Compare(vimFunctions[i].name, name) >= 0
	})
	if i >= len(vimFunctions) {
		return nil
	}
	return &vimFunctions[i]
}

type vimFunc struct {
	name string
	typ  func() vimType
}

func newBufExprType() vimType {
	return newUnionType(typeInt, typeString)
}

func newLnumType() vimType {
	return newUnionType(typeInt, typeString)
}

func newWinnrType() vimType {
	return typeInt
}

func newTabnrType() vimType {
	return typeInt
}

func newPatternType() vimType {
	return typeString
}

func newHandleType() vimType {
	return newUnionType(typeChannel, typeJob)
}

// TODO properties
func newChannelOptionsType() vimType {
	return typeDict
}

// TODO properties
func newChannelOpenOptionsType() vimType {
	return typeDict
}

// TODO properties
func newChannelInfoType() vimType {
	return typeDict
}

// TODO remove this
func newPossibleFuncType() vimType {
	ret := newTypeVar()
	allArgs := make([]vimType, 20)
	for i := range allArgs {
		allArgs[i] = newTypeVar()
	}
	funlist := make([]vimType, 20)
	for i := range funlist {
		funlist[i] = newFuncType(newTupleType(allArgs[:i]...), ret)
	}
	return newUnionType(funlist[0], funlist[1], funlist[2:]...)
}
