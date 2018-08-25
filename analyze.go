package main

import (
	"fmt"
	"reflect"

	"github.com/haya14busa/go-vimlparser/ast"
	"github.com/pkg/errors"
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

var errDupFile = errors.New("duplicate file")

// AddFile adds given file to this file set.
// AddFile returns non-nil error when given filename is already in the file set.
func (s *fileSet) AddFile(filename string, size int64, node *ast.File) error {
	base := s.base + size + 1 // +1 because EOF also has a position
	f := analyFile{s.base, size, filename, node}
	for i := range s.files {
		if s.files[i].name == filename {
			return errDupFile
		}
	}
	s.files = append(s.files, f)
	s.base = base
	return nil
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

func analyze(fset *fileSet) error {
	return (&analyzer{fset, NewScope(nil)}).Run()
}

type analyzer struct {
	fset *fileSet
	nsdb *Scope
}

// Run infers the types of file set.
func (a *analyzer) Run() error {
	var err error
	a.fset.Iterate(func(f *analyFile) bool {
		err = a.inferFile(f.fileNode)
		return err == nil
	})
	return err
}

func (a *analyzer) inferFile(node *ast.File) error {
	// TODO Convert nodes to SSA form
	// https://dev.to/miura1729/-2376
	return a.inferExpr(node)
}

// inferExpr infers the types of expressions in given file node.
func (a *analyzer) inferExpr(node *ast.File) error {
	// Infer the types of literal
	base := &visitorBase{
		e:           nil,
		fset:        a.fset,
		nodeInfoMap: make(map[setPos][]nodeInfo),
		inferQueue:  make([]ast.Node, 0),
		scope:       a.nsdb,
	}
	v := &fileVisitor{base}
	ast.Walk(v, node)
	return base.e
}

type visitorBase struct {
	e           error
	fset        *fileSet
	nodeInfoMap map[setPos][]nodeInfo
	inferQueue  []ast.Node
	scope       *Scope
}

func (base *visitorBase) err(cause error, msg string) ast.Visitor {
	base.e = errors.Wrap(cause, msg)
	return nil
}

func (base *visitorBase) log(node ast.Node) {
	fmt.Printf("node = (%+v) %+v\n", reflect.TypeOf(node), node)
}

var errNoSuchNode = errors.New("no such node in file set")

func (base *visitorBase) determine(node ast.Node, typ nodeType) error {
	npos := node.Pos()
	f := base.fset.File(npos)
	if f == nil {
		return errNoSuchNode
	}
	pos := setPos(f.base + int64(npos.Offset))
	base.nodeInfoMap[pos] = append(base.nodeInfoMap[pos], nodeInfo{node, typ})
	return nil
}

func (base *visitorBase) determineLater(node ast.Node) error {
	base.inferQueue = append(base.inferQueue, node)
	return base.determine(node, unknownType)
}

func (base *visitorBase) pushScope() {
	base.scope = NewScope(base.scope)
}

func (base *visitorBase) popScope() {
	base.scope = base.scope.Outer
}

var errUnmatchedNode = func(node ast.Node) error {
	return errors.Errorf("unmatched node: (%+v) %+v", reflect.TypeOf(node), node)
}

// fileVisitor is an ast.Visitor for ast.File .
type fileVisitor struct {
	*visitorBase
}

func (v *fileVisitor) Visit(node ast.Node) ast.Visitor {
	v.log(node)
	if node == nil {
		return nil // end of visit
	}
	if _, ok := node.(*ast.File); !ok {
		return v.err(errUnmatchedNode(node), "file")
	}
	if err := v.determine(node, noType); err != nil {
		return v.err(err, "file")
	}
	return &stmtVisitor{v.visitorBase, v}
}

// stmtVisitor is an ast.Visitor for ast.Statement .
type stmtVisitor struct {
	*visitorBase
	parent ast.Visitor
}

func (v *stmtVisitor) Visit(node ast.Node) ast.Visitor {
	v.log(node)
	if node == nil {
		return v.parent
	}
	switch node.(type) {
	case *ast.Function:
		if err := v.determine(node, noType); err != nil {
			return v.err(err, "statement")
		}
		v.pushScope()
		return &functionVisitor{v.visitorBase, v}
	case *ast.Let:
		if err := v.determine(node, noType); err != nil {
			return v.err(err, "statement")
		}
		return &letVisitor{v.visitorBase, v}
	case *ast.EchoCmd:
		if err := v.determine(node, noType); err != nil {
			return v.err(err, "statement")
		}
		return &echoCmdVisitor{v.visitorBase, v}
	case ast.ExCommand:
		if err := v.determine(node, noType); err != nil {
			return v.err(err, "statement")
		}
		return &exCommandVisitor{v.visitorBase, v}
	case ast.Statement:
		if err := v.determine(node, noType); err != nil {
			return v.err(err, "statement")
		}
		return v
	default:
		// Given node must be ast.Statement
		return v.err(errUnmatchedNode(node), "statement")
	}
}

// functionVisitor is an ast.Visitor for ast.Let .
type functionVisitor struct {
	*visitorBase
	parent ast.Visitor
}

func (v *functionVisitor) Visit(node ast.Node) ast.Visitor {
	v.log(node)
	if node == nil {
		v.popScope()
		return v.parent
	}
	// TODO
	return v
}

// letVisitor is an ast.Visitor for ast.Let .
type letVisitor struct {
	*visitorBase
	parent ast.Visitor
}

func (v *letVisitor) Visit(node ast.Node) ast.Visitor {
	v.log(node)
	if node == nil {
		return v.parent
	}
	switch node.(type) {
	case ast.Expr:
		if err := v.determineLater(node); err != nil {
			return v.err(err, "let")
		}
		return v
	default:
		// Given node must be ast.Expr
		return v.err(errUnmatchedNode(node), "let")
	}
}

// echoCmdVisitor is an ast.Visitor for ast.Let .
type echoCmdVisitor struct {
	*visitorBase
	parent ast.Visitor
}

func (v *echoCmdVisitor) Visit(node ast.Node) ast.Visitor {
	v.log(node)
	if node == nil {
		return v.parent
	}
	switch node.(type) {
	case ast.Expr:
		if err := v.determineLater(node); err != nil {
			return v.err(err, "echo")
		}
		return v
	default:
		// Given node must be ast.Expr
		return v.err(errUnmatchedNode(node), "echo")
	}
}

// exCommandVisitor is an ast.Visitor for ast.Let .
type exCommandVisitor struct {
	*visitorBase
	parent ast.Visitor
}

func (v *exCommandVisitor) Visit(node ast.Node) ast.Visitor {
	v.log(node)
	if node == nil {
		return v.parent
	}
	return v
}

type nodeType int

const (
	noType nodeType = iota
	unknownType
	voidType

	intType
	floatType
	stringType

	listType
	dictType
	tupleType
	unionType
)

type nodeInfo struct {
	ast.Node
	typ nodeType
}

// NewScope is the constructor for Scope.
func NewScope(outer *Scope) *Scope {
	return &Scope{outer, nil}
}

// ObjKind is the types of object.
type ObjKind int

// The list of possible Object kinds.
const (
	Bad ObjKind = iota // for error handling
	Pkg                // package
	Con                // constant
	Typ                // type
	Var                // variable
	Fun                // function or method
	Lbl                // label
)

// An Object describes a named language entity such as a package,
// constant, type, variable, function (incl. methods), or label.
//
// The Data fields contains object-specific data:
//
//	Kind    Data type         Data value
//	Pkg     *Scope            package scope
//	Con     int               iota for the respective declaration
//
type Object struct {
	Kind ObjKind
	Name string      // declared name
	Data interface{} // object-specific data; or nil
	Type interface{} // placeholder for type information; may be nil
}

// Scope holds variables.
type Scope struct {
	Outer   *Scope
	Objects map[string]*Object
}

// insert attempts to insert a named object obj into the scope s. If the scope
// already contains an object alt with the same name, insert leaves the scope
// unchanged and returns alt. Otherwise it inserts obj and returns nil.
func (s *Scope) insert(obj *Object) (alt *Object) {
	var ok bool
	alt, ok = s.Objects[obj.Name]
	if ok {
		return
	}
	s.Objects[obj.Name] = obj
	return nil
}

func (s *Scope) lookup(name string) *Object {
	return s.Objects[name]
}
