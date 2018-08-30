package main

import (
	"fmt"
	"unicode"

	"github.com/haya14busa/go-vimlparser/ast"
)

type nodeDB struct {
	scope   *scope
	nodeMap nodeMap
}

func newNodeDB() *nodeDB {
	return &nodeDB{newScope(nil), make(nodeMap)}
}

type identInfo struct {
	ident *ast.Ident
	cname *CName
	kind  nodeKind
	typ   vimType
}

// setLHS is called when ast.Let or ast.For is found.
func (db *nodeDB) setLHS(left ast.Expr, list []ast.Expr, rest ast.Expr) {
	if left != nil {
		if id, ok := left.(*ast.Ident); ok {
			typ := newTypeVar()
			kind := kindVarname
			cname := db.newCNameByName(id.Name, kind.isFunc())
			info := identInfo{id, cname, kind, typ}
			db.setIdentInfo(info)
		}
	}
	for i := range list {
		db.setLHS(list[i], nil, nil)
	}
	if rest != nil {
		db.setLHS(rest, nil, nil)
	}
}

func (db *nodeDB) setFuncParams(params []*ast.Ident) {
	for _, id := range params {
		// TODO get argument type from comment before function node
		typ := typeAny
		kind := kindVarname
		cname := db.newCNameByName("a:"+id.Name, false)
		info := identInfo{id, cname, kind, typ}
		db.setIdentInfo(info)
	}
}

// newCNameByName converts given non-canonical name to canonical name.
// TODO (SSA) Add version to:
// * local variable name in a function
// * global variable name in a toplevel
func (db *nodeDB) newCNameByName(name string, isFunc bool) (cname *CName) {
	if name == "" {
		return nil
	}

	if isFunc {
		// len(name) > 2 skips dict variable like "g:"
		noScope := len(name) < 2 || len(name) > 2 && name[1] != ':'
		if noScope { // No scope. is it a vim built-in function?
			cname = getVimFuncCName(name)
			if cname != nil {
				return cname
			}
			// Otherwise, it is an error.
			return nil
		}
		// E128: Function name must start with a capital or "s:"
		var scope string
		switch {
		case name[0] <= unicode.MaxASCII && unicode.IsUpper(rune(name[0])):
			scope = "g"
		case name[:2] == "s:":
			scope = "s"
			name = name[2:]
		default:
			return nil
		}
		return newCName(scope, name)
	}

	scope, name := splitVimVar(name)
	if scope == "" {
		if db.scope.outer == nil { // is at toplevel
			scope = "g"
		} else { // in a function
			scope = "l"
		}
	}
	return newCName(scope, name)
}

// If id is a known node, return the information.
// Otherwise it tries to look up a function (isFunc == true) or
// a variable (isFunc == false) from database.
func (db *nodeDB) getIdentInfo(id *ast.Ident, isFunc bool) *identInfo {
	if db.knownNode(id) {
		kind := db.getKind(id)
		cname := db.newCNameByName(id.Name, kind.isFunc())
		info := db.getBuiltinIdentInfo(id, isFunc, cname)
		if info != nil {
			return info
		}
		return &identInfo{id, cname, kind, db.getType(id)}
	}

	cname := db.newCNameByName(id.Name, isFunc)
	if cname == nil {
		return nil
	}

	info := db.getBuiltinIdentInfo(id, isFunc, cname)
	if info != nil {
		return info
	}

	if cname.scope == "l" || cname.scope == "a" {
		node, scope := db.scope.lookup(cname)
		if scope != nil {
			return &identInfo{id, cname, db.getKind(node), db.getType(node)}
		}
	} else {
		// Other scope variables are defined somewhere...
		// FIXME what should I do? ('_'
		return &identInfo{id, cname, kindVarname, typeAny}
	}

	return nil
}

func (db *nodeDB) getBuiltinIdentInfo(id *ast.Ident, isFunc bool, cname *CName) *identInfo {
	if cname == nil {
		return nil
	}
	if isFunc {
		if cname.scope == "vimfunc" {
			return &identInfo{id, cname, kindFunCall, getVimFuncType(cname)}
		}
	} else {
		if cname.scope == "v" {
			return &identInfo{id, cname, kindVarname, getVimVarType(cname)}
		}
	}
	return nil
}

// setIdentInfo sets given identifier information to node database.
// * Adds the identifier to the current scope.
// * Adds niCanonName entry to nodeInfo.
// * Adds niIdentKind entry to nodeInfo.
// * Adds niVimType entry to nodeInfo.
// If the variable/function is added already, overwrites previous node kind.
func (db *nodeDB) setIdentInfo(info identInfo) {
	db.scope.set(info.cname, info.ident)
	db.setCanonName(info.ident, info.cname)
	db.setKind(info.ident, info.kind)
	db.setType(info.ident, info.typ)
}

func (db *nodeDB) knownNode(node ast.Node) bool {
	return db.getKind(node) != kindUnknown
}

func (db *nodeDB) setCanonName(node ast.Node, cname *CName) {
	info := db.nodeMap.get(node)
	info[niCanonName] = cname
}

func (db *nodeDB) getCanonName(node ast.Node) *CName {
	info := db.nodeMap.get(node)
	if info[niCanonName] != nil {
		if cname, ok := info[niCanonName].(*CName); ok {
			return cname
		}
	}
	return nil
}

func (db *nodeDB) setKind(node ast.Node, kind nodeKind) {
	info := db.nodeMap.get(node)
	info[niIdentKind] = &kind
}

func (db *nodeDB) getKind(node ast.Node) nodeKind {
	info := db.nodeMap.get(node)
	if info[niIdentKind] != nil {
		if v, ok := info[niIdentKind].(*nodeKind); ok {
			return *v
		}
	}
	return kindUnknown
}

func (db *nodeDB) setType(node ast.Node, typ vimType) {
	info := db.nodeMap.get(node)
	info[niVimType] = typ
}

func (db *nodeDB) getType(node ast.Node) vimType {
	info := db.nodeMap.get(node)
	if info[niVimType] != nil {
		if v, ok := info[niVimType].(vimType); ok {
			return v
		}
	}
	return typeUnknown
}

func (db *nodeDB) pushScope() {
	db.scope = newScope(db.scope)
}

func (db *nodeDB) popScope() {
	db.scope = db.scope.outer
}

type nodeInfo map[nodeInfoKey]interface{}
type nodeMap map[ast.Node]nodeInfo

// has checks only the entry is non-nil or not.
// Empty entries must not exist.
func (m nodeMap) has(node ast.Node) bool {
	return m[node] != nil
}

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

func (kind nodeKind) isFunc() bool {
	return kind == kindFunDecl || kind == kindFunCall
}

// CName is a FQDN string which points to the single identifier (variable or function).
// Format is (scope is one of "l", "a"):
//
//	{scope}:{varname}
//
// TODO: Currently no cname information for the scopes of "b:", "w:", "t:", "g:", "s:", "v:".
//
type CName struct {
	scope   string
	varname string
}

func newCName(scope, varname string) *CName {
	return &CName{scope, varname}
}

func (cn *CName) String() string {
	return fmt.Sprintf("%s:%s", cn.scope, cn.varname)
}

type scope struct {
	outer *scope
	nodes map[string]ast.Node
}

func newScope(outer *scope) *scope {
	return &scope{outer, make(map[string]ast.Node)}
}

func (s *scope) set(cname *CName, node ast.Node) {
	s.nodes[cname.String()] = node
}

func (s *scope) get(cname *CName) (node ast.Node, exists bool) {
	if cname == nil {
		return
	}
	node, exists = s.nodes[cname.String()]
	return
}

// lookup returns non-nil node and scope if it is found.
func (s *scope) lookup(cname *CName) (ast.Node, *scope) {
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
