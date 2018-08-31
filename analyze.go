package main

import (
	"fmt"

	"github.com/haya14busa/go-vimlparser/ast"
	"github.com/haya14busa/go-vimlparser/token"
)

type fileSet struct {
	files []analyFile
	last  *analyFile
	base  int64
}

func newFileSet(cap int) *fileSet {
	return &fileSet{
		files: make([]analyFile, 0, cap),
		base:  1, // 0 == NoPos
	}
}

// addFile adds given file to this file set.
// addFile returns non-nil error when given filename is already in the file set.
func (s *fileSet) addFile(filename string, size int64, node *ast.File) {
	base := s.base + size + 1 // +1 because EOF also has a position
	f := analyFile{s.base, size, filename, node}
	s.files = append(s.files, f)
	s.base = base
}

// File returns *analyFile by filename.
func (s *fileSet) File(pos ast.Pos) *analyFile {
	if s.last != nil && s.last.name == pos.Filename {
		return s.last
	}
	if pos.Filename == "" {
		return nil
	}
	for i := range s.files {
		if s.files[i].name == pos.Filename {
			s.last = &s.files[i]
			return &s.files[i]
		}
	}
	return nil
}

func (s *fileSet) Iterate(f func(*analyFile) bool) {
	for i := range s.files {
		if !f(&s.files[i]) {
			break
		}
	}
}

type analyFile struct {
	base     int64
	size     int64
	name     string
	fileNode *ast.File
}

type setPos int64 // noPos == 0

func (f *analyFile) toSetPos() setPos {
	return setPos(f.base + f.size)
}

func newAnalyzer(fset *fileSet) *analyzer {
	return &analyzer{fset, newNodeDB()}
}

type analyzer struct {
	fset   *fileSet
	nodeDB *nodeDB
}

// run infers the types of file set.
func (a *analyzer) run() error {
	var err error
	a.fset.Iterate(func(f *analyFile) bool {
		err = a.addNodeInfo(f.fileNode)
		return err == nil
	})
	return err
}

func (a *analyzer) addNodeInfo(node *ast.File) error {
	v := &nodeInfoVisitor{nodeDB: a.nodeDB}
	ast.Walk(v, node)
	return v.err
}

// nodeInfoVisitor visits nodes and adds some information in nodeInfo.
// See nodeInfoKey for details.
type nodeInfoVisitor struct {
	nodeDB *nodeDB
	err    error
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
	if node == nil || v.nodeDB.knownNode(node) {
		return v
	}
	switch n := node.(type) {
	case *ast.File:

	case *ast.Comment:

	case *ast.Excmd:

	case *ast.Function:
		// Build vim type of the function.
		// TODO get return type from comment before function node
		argsTypes := make([]vimType, len(n.Params))
		for i := range argsTypes {
			argsTypes[i] = newTypeVar()
		}
		// Set type, kind to function name node.
		typ := newFuncType(newTupleType(argsTypes...), newTypeVar())
		if id, ok := n.Name.(*ast.Ident); ok {
			kind := kindFunDecl
			cname, err := v.nodeDB.newCNameByName(id, id.Name, true)
			if err != nil {
				v.err = err
				return nil
			}
			info := identInfo{id, cname, kind, typ}
			v.nodeDB.setIdentInfo(info)
		} else {
			// TODO support method: obj.func()
			// TODO support curly-braces-names ?
		}
		// Add parameter identifiers to inner scope.
		v.nodeDB.pushScope()
		v.err = v.nodeDB.setFuncParams(n.Params)
		if v.err != nil {
			return nil
		}

	case *ast.EndFunction:
		v.nodeDB.popScope()

	case *ast.DelFunction:

	case *ast.Return:

	case *ast.ExCall:

	case *ast.Let:
		v.err = v.nodeDB.setLHS(n.Left, n.List, n.Rest)
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
		v.err = v.nodeDB.setLHS(n.Left, n.List, n.Rest)
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
				info, err := v.nodeDB.lookUpIdentInfo(id, stFunc+stFromScope)
				if err != nil {
					v.err = err
					return false
				}
				// Change kind to kindFunCall
				newInfo := identInfo{id, info.cname, kindFunCall, info.typ}
				v.nodeDB.setIdentInfo(newInfo)
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
		v.nodeDB.setKind(n.Right, kindProp)

	case *ast.BasicLit:
		switch n.Kind {
		case token.NUMBER:
			v.nodeDB.setType(n, typeInt)
			v.nodeDB.setKind(n, kindLiteral)
		case token.STRING:
			v.nodeDB.setType(n, typeString)
			v.nodeDB.setKind(n, kindLiteral)
		case token.OPTION:
			v.nodeDB.setType(n, getTypeOfVimOption(n.Value))
			v.nodeDB.setKind(n, kindLiteral)
		case token.ENV:
			v.nodeDB.setType(n, typeString)
			v.nodeDB.setKind(n, kindLiteral)
		case token.REG:
			v.nodeDB.setType(n, typeString)
			v.nodeDB.setKind(n, kindLiteral)
		}

	case *ast.List:
		v.nodeDB.setType(n, typeList)
		v.nodeDB.setKind(n, kindLiteral)

	case *ast.Dict:
		v.nodeDB.setType(n, typeDict)
		v.nodeDB.setKind(n, kindLiteral)

	case *ast.Ident:
		// Check unmarked identifier nodes.
		// It must be refering to a existing variable or a function.
		info, err := v.nodeDB.lookUpIdentInfo(n, stFunc+stVar+stFromScope)
		if err != nil {
			v.err = wrapError(err, n, "could not look up the identifier")
			return nil
		}
		if !v.nodeDB.knownNode(n) {
			v.nodeDB.setIdentInfo(*info)
		}

	case *ast.CurlyName:

	case *ast.CurlyNameLit:

	case *ast.CurlyNameExpr:

	case *ast.LambdaExpr:
		// TODO get return type from comment before lambda node
		argsTypes := make([]vimType, len(n.Params))
		for i := range argsTypes {
			argsTypes[i] = newTypeVar()
		}
		typ := newFuncType(newTupleType(argsTypes...), newTypeVar())
		v.nodeDB.setType(n, typ)
		v.nodeDB.setKind(n, kindLiteral)

	case *ast.ParenExpr:

	default:
		panic(fmt.Sprintf("nodeInfoVisitor.Visit(): unexpected node type %T", n))
	}
	return v
}

// checkRefIdents checks identifiers of rhs.
// checkRefIdents marks node as read, by calling nodeDB.setKind().
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
