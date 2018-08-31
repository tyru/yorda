package main

import (
	"fmt"
	"strconv"
	"unicode"

	"github.com/haya14busa/go-vimlparser/ast"
)

type fileNodeInfo struct {
	filename string
	scope    *scope
	nodeMap  nodeMap
}

func newFileNodeInfo(filename string) *fileNodeInfo {
	return &fileNodeInfo{filename, newScope(nil), make(nodeMap)}
}

type identInfo struct {
	ident *ast.Ident
	cname *CName
	kind  nodeKind
	typ   vimType
}

// setLHS is called when ast.Let or ast.For is found.
func (db *fileNodeInfo) setLHS(left ast.Expr, list []ast.Expr, rest ast.Expr) error {
	if left != nil {
		if id, ok := left.(*ast.Ident); ok {
			typ := newTypeVar()
			kind := kindVarname
			cname, err := db.newCNameByName(id, id.Name, false)
			if err != nil {
				return err
			}
			info := identInfo{id, cname, kind, typ}
			db.scope.set(info.cname, info.ident)
			db.setIdentInfo(info)
		}
	}
	for i := range list {
		if err := db.setLHS(list[i], nil, nil); err != nil {
			return err
		}
	}
	if rest != nil {
		if err := db.setLHS(rest, nil, nil); err != nil {
			return err
		}
	}
	return nil
}

func (db *fileNodeInfo) setFuncParams(params []*ast.Ident) error {
	for i, id := range params {
		// TODO get argument type from comment before function node
		typ := newTypeVar()
		kind := kindVarname
		// a:name
		cname, err := db.newCNameByName(id, "a:"+id.Name, false)
		if err != nil {
			return err
		}
		info := identInfo{id, cname, kind, typ}
		db.scope.set(info.cname, info.ident)
		db.setIdentInfo(info)
		// a:1, a:2, ...
		cname, err = db.newCNameByName(id, "a:"+strconv.Itoa(i+1), false)
		if err != nil {
			return err
		}
		info = identInfo{id, cname, kind, typ}
		db.scope.set(info.cname, info.ident)
		db.setIdentInfo(info)
	}
	return nil
}

// newCNameByName converts given non-canonical name to canonical name.
// It returns non-nil error for:
// * Invalid name (name == "")
// * No built-in function (name == "hoge" && isFunc == true)
// * function name must start with a capital or 's:'
//
// TODO (SSA) Add version to:
// * local variable name in a function
// * global variable name in a toplevel
func (db *fileNodeInfo) newCNameByName(id *ast.Ident, name string, isFunc bool) (cname *CName, err error) {
	if name == "" {
		typ := "variable"
		if isFunc {
			typ = "function"
		}
		return nil, errorf(id, "invalid %s name: %s", typ, name)
	}

	if isFunc {
		// len(name) > 2 skips dict variable like "g:"
		noScope := len(name) < 2 || len(name) > 2 && name[1] != ':'
		if noScope { // No scope. is it a vim built-in function?
			cname = getVimFuncCName(name)
			if cname != nil {
				return cname, nil
			}
			// Otherwise, it is an error.
			return nil, errorf(id, "no such built-in function: %s", name)
		}
		var scope string
		switch {
		case name[0] <= unicode.MaxASCII && unicode.IsUpper(rune(name[0])):
			scope = "g"
		case name[:2] == "s:":
			scope = "s"
			name = name[2:]
		default:
			// :help E128
			return nil, errorf(id, "function name must start with a capital or 's:': %s", name)
		}
		return newCName(scope, name, db.filename), nil
	}

	scope, name := splitVimVar(name)
	if scope == "" {
		if db.scope.outer == nil { // is at toplevel
			scope = "g"
		} else { // in a function
			scope = "l"
		}
	}
	return newCName(scope, name, db.filename), nil
}

// searchTarget changes the behavior of fileNodeInfo.lookUpIdentInfo().
//
//	db.lookUpIdentInfo(id, stFunc)    // search the function
//	db.lookUpIdentInfo(id, stVar)    // search the variable
//	db.lookUpIdentInfo(id, stFunc+stVar)    // search the function or the variable
//	db.lookUpIdentInfo(id, stFunc+stFromScope)    // search the function from scope
//
type searchTarget int

const (
	stVar searchTarget = 1 << iota
	stFunc
	stFromScope
)

// If id is a known node, return the information.
// Otherwise it tries to look up a function (isFunc == true) or
// a variable (isFunc == false) from database.
func (db *fileNodeInfo) lookUpIdentInfo(id *ast.Ident, target searchTarget) (info *identInfo, err error) {
	list := make([]bool, 0, 2)
	if target&stFunc != 0 {
		list = append(list, true)
	}
	if target&stVar != 0 {
		list = append(list, false)
	}

	if db.knownNode(id) {
		kind := db.getKind(id)
		if kind == kindUnknown {
			panic(errorf(id, "fatal: node is known but no kind?: node (%T) = %+v", id, id).Error())
		}
		cname := db.getCanonName(id)
		if kind != kindProp && cname == nil {
			panic(errorf(id, "fatal: node is known but no cname?: node (%T) = %+v", id, id).Error())
		}
		for _, isFunc := range list {
			info = db.getBuiltinIdentInfo(id, isFunc, cname)
			if info != nil {
				return info, nil
			}
		}
		info = &identInfo{id, cname, db.getKind(id), db.getType(id)}
		return info, nil
	}

	if target&stFromScope == 0 {
		return nil, errorf(id, "the node is not known node: %s", id.Name)
	}

	var cname *CName
	for _, isFunc := range list {
		cname, err = db.newCNameByName(id, id.Name, isFunc)
		if err != nil {
			continue
		}
		info = db.getBuiltinIdentInfo(id, isFunc, cname)
		if info != nil {
			return info, nil
		}
		err = nil
		break
	}
	if err != nil {
		return nil, err
	}

	if cname.scope == "l" || cname.scope == "a" {
		node, scope := db.scope.lookup(cname)
		if scope != nil {
			typ := db.getType(node)
			if typ == typeUnknown {
				panic(errorf(id, "node's type is unknown\n").Error())
			}
			kind := db.getKind(node)
			if kind == kindUnknown {
				panic(errorf(id, "node's kind is unknown\n").Error())
			}
			info = &identInfo{id, cname, kind, typ}
			return info, nil
		}
		return nil, errorf(id, "unresolved reference %s", id.Name)
	}

	// Other scope variables are defined somewhere...
	// TODO load plugin/*.vim before other *.vim files?
	info = &identInfo{id, cname, kindVarname, newTypeVar()}
	return info, nil
}

func (db *fileNodeInfo) getBuiltinIdentInfo(id *ast.Ident, isFunc bool, cname *CName) *identInfo {
	if cname == nil {
		return nil
	}
	if isFunc {
		if cname.scope == "vimfunc" {
			return &identInfo{id, cname, kindFunCall, getVimFuncType(cname)}
		}
	} else {
		if isVimVar(cname.scope, cname.varname) {
			return &identInfo{id, cname, kindVarname, newTypeVar()}
		}
	}
	return nil
}

// setIdentInfo sets given identifier information to node database.
// * setIdentInfo *DOESN'T* add the identifier to the current scope.
// * setIdentInfo adds niCanonName entry to nodeInfo.
// * setIdentInfo adds niIdentKind entry to nodeInfo.
// * setIdentInfo adds niVimType entry to nodeInfo.
// If the variable/function is added already, overwrites previous node kind.
func (db *fileNodeInfo) setIdentInfo(info identInfo) {
	db.setCanonName(info.ident, info.cname)
	db.setKind(info.ident, info.kind)
	db.setType(info.ident, info.typ)
}

func (db *fileNodeInfo) knownNode(node ast.Node) bool {
	return db.getKind(node) != kindUnknown
}

func (db *fileNodeInfo) setCanonName(node ast.Node, cname *CName) {
	info := db.nodeMap.get(node)
	info[niCanonName] = cname
}

func (db *fileNodeInfo) getCanonName(node ast.Node) *CName {
	info := db.nodeMap.get(node)
	if info[niCanonName] != nil {
		if cname, ok := info[niCanonName].(*CName); ok {
			return cname
		}
	}
	return nil
}

func (db *fileNodeInfo) setKind(node ast.Node, kind nodeKind) {
	info := db.nodeMap.get(node)
	info[niIdentKind] = &kind
}

func (db *fileNodeInfo) getKind(node ast.Node) nodeKind {
	info := db.nodeMap.get(node)
	if info[niIdentKind] != nil {
		if v, ok := info[niIdentKind].(*nodeKind); ok {
			return *v
		}
	}
	return kindUnknown
}

func (db *fileNodeInfo) setType(node ast.Node, typ vimType) {
	info := db.nodeMap.get(node)
	info[niVimType] = typ
}

func (db *fileNodeInfo) getType(node ast.Node) vimType {
	info := db.nodeMap.get(node)
	if info[niVimType] != nil {
		if v, ok := info[niVimType].(vimType); ok {
			return v
		}
	}
	return typeUnknown
}

func (db *fileNodeInfo) pushScope() {
	db.scope = newScope(db.scope)
}

func (db *fileNodeInfo) popScope() {
	db.scope = db.scope.outer
}

type nodeInfo map[nodeInfoKey]interface{}
type nodeMap map[ast.Node]nodeInfo

func (m nodeMap) get(node ast.Node) nodeInfo {
	info := m[node]
	if info == nil {
		m[node] = make(nodeInfo, niMaxNodeInfoKey)
		info = m[node]
	}
	return info
}

type nodeInfoKey int

const (
	// What is the vim type of this identifier? (map value: vimType)
	niVimType nodeInfoKey = iota

	// What is the type of this identifier? (map value: nodeKind)
	niIdentKind

	// the variable name of static single assignment form (map value: CName)
	niCanonName

	// for the capacity to make(nodeInfoKey)
	niMaxNodeInfoKey
)

type nodeKind int

const (
	kindUnknown nodeKind = iota
	kindLiteral          // literal
	kindVarname          // variable
	kindFunDecl          // function declaration (:function ident())
	kindFunCall          // function call (ident())
	kindProp             // property access
)

// CName is a FQDN string (cname.String()) which points to
// the single identifier (variable or function).
//
// scope is "s", "l" or "a":
//
//	{filename}:{scope}:{varname}
//
// scope is "b:", "w:", "t:", "g:" or "v:":
//
//	{scope}:{varname}
//
type CName struct {
	scope    string
	varname  string
	filename string
}

func newCName(scope, varname, filename string) *CName {
	return &CName{scope, varname, filename}
}

func (cn *CName) String() string {
	if cn.scope == "l" || cn.scope == "a" || cn.scope == "s" {
		return fmt.Sprintf("%s:%s:%s", cn.filename, cn.scope, cn.varname)
	}
	return fmt.Sprintf("%s:%s", cn.scope, cn.varname)
}

type scope struct {
	outer *scope
	nodes map[string]*ast.Ident
}

func newScope(outer *scope) *scope {
	return &scope{outer, make(map[string]*ast.Ident)}
}

func (s *scope) set(cname *CName, node *ast.Ident) {
	s.nodes[cname.String()] = node
}

func (s *scope) get(cname *CName) (node *ast.Ident, exists bool) {
	if cname == nil {
		return
	}
	node, exists = s.nodes[cname.String()]
	return
}

// lookup returns non-nil node and scope if it is found.
func (s *scope) lookup(cname *CName) (*ast.Ident, *scope) {
	if cname == nil {
		return nil, nil
	}
	for scope := s; scope != nil; scope = scope.outer {
		if node, ok := scope.get(cname); ok {
			return node, scope
		}
	}
	return nil, nil
}
