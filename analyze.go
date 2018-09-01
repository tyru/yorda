package main

import (
	"fmt"
	"strings"
	"unicode/utf8"

	"github.com/haya14busa/go-vimlparser/ast"
	"github.com/haya14busa/go-vimlparser/token"
)

func newAnalyzer(fset *fileSet) *analyzer {
	return &analyzer{fset}
}

type analyzer struct {
	fset *fileSet
}

// run infers the types of file set.
func (a *analyzer) run() error {
	var err error
	a.fset.iterate(func(f *analyFile) bool {
		if f.info == nil {
			f.info = newFileNodeInfo(f.name)
		}
		err = a.addNodeInfo(f)
		return err == nil
	})
	return err
}

func (a *analyzer) addNodeInfo(f *analyFile) error {
	v := &nodeInfoVisitor{info: f.info, prevComments: make([]string, 0, 32)}
	ast.Walk(v, f.node)
	return v.err
}

// nodeInfoVisitor visits nodes and adds some information in nodeInfo.
// See nodeInfoKey for details.
type nodeInfoVisitor struct {
	info         *fileNodeInfo
	err          error
	prevComments []string
}

// ast.BasicLit:
//   * type = the type of the literal
//   * kind = kindLiteral
// ast.CallExpr:
//   * type = the type of the function
//   * kind = kindFunCall
// ast.Function:
//   * type = the type of the function
//   * kind = kindFunDecl
// ast.Let, ast.For:
//   * lhs
//     * type = the type of the expression which lhs varname refers to
//     * kind = kindVarname
//   * rhs
//     * type = the type of the variable
//     * kind = kindVarname
// ast.Ident: It must be refering to a existing variable or a function.
//   * type = <existing variable/function>.type
//   * kind = <existing variable/function>.kind
// * If the node is *ast.Function, determine the type of function.
func (v *nodeInfoVisitor) Visit(node ast.Node) ast.Visitor {
	if node == nil || v.info.knownNode(node) {
		return v
	}
	var comment *ast.Comment

	switch n := node.(type) {
	case *ast.File:

	case *ast.Comment:
		comment = n

	case *ast.Excmd:

	case *ast.Function:
		// Build vim type of the function.
		// Get return type from comment before function node
		// if the previous node was comment node.
		var typ vimType
		if len(v.prevComments) > 0 {
			typ = v.getFuncTypeFromComment(v.prevComments, n)
			if typ == nil {
				typ = v.getTypeOfFunction(n.Params)
			}
		} else {
			typ = v.getTypeOfFunction(n.Params)
		}
		if id, ok := n.Name.(*ast.Ident); ok {
			kind := kindFunDecl
			cname, err := v.info.newCNameByName(id, id.Name, true)
			if err != nil {
				v.err = err
				return nil
			}
			info := identInfo{id, cname, kind, typ}
			v.info.setIdentInfo(info)
		} else {
			// TODO support method: obj.func()
			// TODO support curly-braces-names ?
		}
		// Add parameter identifiers to inner scope.
		v.info.pushScope()
		v.err = v.info.setFuncParams(n.Params)
		if v.err != nil {
			return nil
		}

	case *ast.EndFunction:
		v.info.popScope()

	case *ast.DelFunction:

	case *ast.Return:

	case *ast.ExCall:

	case *ast.Let:
		v.err = v.info.setLHS(n.Left, n.List, n.Rest)
		if v.err != nil {
			return nil
		}
		v.err = v.checkRefIdents(n.Right)
		if v.err != nil {
			return nil
		}

	case *ast.UnLet:

	case *ast.LockVar:

	case *ast.UnLockVar:

	case *ast.If:

	case *ast.ElseIf:

	case *ast.Else:

	case *ast.EndIf:

	case *ast.While:

	case *ast.EndWhile:

	case *ast.For:
		v.err = v.info.setLHS(n.Left, n.List, n.Rest)
		if v.err != nil {
			return nil
		}
		v.err = v.checkRefIdents(n.Right)
		if v.err != nil {
			return nil
		}

	case *ast.EndFor:

	case *ast.Continue:

	case *ast.Break:

	case *ast.Try:

	case *ast.Catch:

	case *ast.Finally:

	case *ast.EndTry:

	case *ast.Throw:

	case *ast.EchoCmd:

	case *ast.Echohl:

	case *ast.Execute:

	case *ast.TernaryExpr:

	case *ast.BinaryExpr:

	case *ast.UnaryExpr:

	case *ast.SubscriptExpr:

	case *ast.SliceExpr:

	case *ast.CallExpr:
		ast.Inspect(n.Fun, func(n ast.Node) bool {
			// Register identifiers in n.Fun to be able to look up later.
			if v.Visit(n) == nil {
				if v.err == nil {
					v.err = errorf(n, "fatal: v.Visit() returns nil but v.err == nil")
				}
				return false
			}
			// Set ident information of the left-most identifier.
			if id, ok := n.(*ast.Ident); ok {
				info, err := v.info.lookUpIdentInfo(id, stFunc+stFromScope)
				if err != nil {
					v.err = err
					return false
				}
				// Change kind to kindFunCall
				newInfo := identInfo{id, info.cname, kindFunCall, info.typ}
				v.info.setIdentInfo(newInfo)
				return true
			}
			return true
		})
		if v.err != nil {
			return nil
		}

	case *ast.DotExpr:
		// TODO check variable in n.Left
		// FIXME kindProp.isFunc() returns false, but obj.prop may be a function!
		v.info.setKind(n.Right, kindProp)

	case *ast.BasicLit:
		switch n.Kind {
		case token.NUMBER:
			v.info.setType(n, typeInt)
			v.info.setKind(n, kindLiteral)
		case token.STRING:
			v.info.setType(n, typeString)
			v.info.setKind(n, kindLiteral)
		case token.OPTION:
			v.info.setType(n, getTypeOfVimOption(n.Value))
			v.info.setKind(n, kindLiteral)
		case token.ENV:
			v.info.setType(n, typeString)
			v.info.setKind(n, kindLiteral)
		case token.REG:
			v.info.setType(n, typeString)
			v.info.setKind(n, kindLiteral)
		}

	case *ast.List:
		v.info.setType(n, typeList)
		v.info.setKind(n, kindLiteral)

	case *ast.Dict:
		v.info.setType(n, typeDict)
		v.info.setKind(n, kindLiteral)

	case *ast.Ident:
		// Check unmarked identifier nodes.
		// It must be refering to a existing variable or a function.
		info, err := v.info.lookUpIdentInfo(n, stFunc+stVar+stFromScope)
		if err != nil {
			v.err = wrapError(err, n, "could not look up the identifier")
			return nil
		}
		if !v.info.knownNode(n) {
			v.info.setIdentInfo(*info)
		}

	case *ast.CurlyName:

	case *ast.CurlyNameLit:

	case *ast.CurlyNameExpr:

	case *ast.LambdaExpr:
		// TODO get return type from comment, which is before the statement node
		// which lambda belongs to.
		typ := v.getTypeOfFunction(n.Params)
		v.info.setType(n, typ)
		v.info.setKind(n, kindLiteral)

	case *ast.ParenExpr:

	default:
		panic(fmt.Sprintf("nodeInfoVisitor.Visit(): unexpected node type %T", n))
	}

	// Merge successive comment nodes into one comment string.
	if comment != nil {
		v.prevComments = append(v.prevComments, comment.Text)
	} else {
		v.prevComments = v.prevComments[:0]
	}

	return v
}

// checkRefIdents checks identifiers of rhs.
// checkRefIdents marks node as read, by calling fileNodeInfo.setKind().
func (v *nodeInfoVisitor) checkRefIdents(node ast.Node) error {
	var err error
	ast.Inspect(node, func(n ast.Node) bool {
		// The node must be seen before.
		if v.Visit(n) == nil {
			if v.err == nil {
				v.err = errorf(n, "fatal: v.Visit() returns nil but v.err == nil")
			}
		}
		return v.err == nil
	})
	return err
}

func (v *nodeInfoVisitor) getTypeOfFunction(params []*ast.Ident) vimType {
	argsTypes := make([]vimType, len(params))
	for i := range argsTypes {
		argsTypes[i] = newTypeVar()
	}
	return newFuncType(newTupleType(argsTypes...), newTypeVar())
}

func (v *nodeInfoVisitor) getFuncTypeFromComment(lines []string, f *ast.Function) vimType {
	var retType vimType
	argsTypes := make(map[string]vimType, len(f.Params))
	for i := range lines {
		l := &typeLexer{input: lines[i]}
		l.acceptRun(" \t")
		if !l.accept("@") {
			continue
		}
		word := l.nextWord()
		if word != "param" && word != "returns" && word != "return" {
			continue
		}
		accepted := l.acceptRun(" \t") && l.accept("{")
		if !accepted {
			continue
		}
		typ := v.parseType(l)
		if typ == nil {
			continue
		}
		if !l.accept("}") {
			continue
		}
		if word == "param" {
			if !l.acceptRun(" \t") {
				continue
			}
			varname := l.nextWord()
			if varname == "" {
				continue
			}
			argsTypes[varname] = typ
		} else {
			retType = typ
		}
	}
	args := make([]vimType, len(f.Params))
	for i, id := range f.Params {
		if argsTypes[id.Name] != nil {
			args[i] = argsTypes[id.Name]
		} else {
			args[i] = newTypeVar()
		}
	}
	if retType == nil {
		retType = newTypeVar()
	}
	return newFuncType(newTupleType(args...), retType)
}

func (v *nodeInfoVisitor) parseType(l *typeLexer) vimType {
	name := l.nextWord()
	typ := getTypeID(name)

	// TODO composite types

	return typ
}

type typeLexer struct {
	input string
	pos   int
	width int
}

const lexEOF = -1

func (l *typeLexer) accept(set string) bool {
	if strings.ContainsRune(set, l.next()) {
		return true
	}
	l.backup()
	return false
}

func (l *typeLexer) acceptRun(set string) bool {
	if strings.ContainsRune(set, l.next()) {
		for strings.ContainsRune(set, l.next()) {
		}
		l.backup()
		return true
	}
	l.backup()
	return false
}

func (l *typeLexer) acceptWord(word string) bool {
	w := l.nextWord()
	if w == word {
		return true
	}
	l.pos -= len(w)
	l.width = 1
	return false
}

func (l *typeLexer) next() rune {
	if l.pos >= len(l.input) {
		l.width = 0
		return lexEOF
	}
	r, w := utf8.DecodeRuneInString(l.input[l.pos:])
	l.width = w
	l.pos += l.width
	return r
}

func (l *typeLexer) backup() {
	l.pos -= l.width
}

func (l *typeLexer) nextWord() string {
	n := 0
	s := l.input[l.pos:]
	for ; n < len(s); n++ {
		if !l.isAlpha(rune(s[n])) {
			break
		}
	}
	l.pos += n
	return s[:n]
}

func (l *typeLexer) isAlpha(r rune) bool {
	return '0' <= r && r <= '9' ||
		'A' <= r && r <= 'Z' ||
		'a' <= r && r <= 'z' ||
		r == '_'
}
