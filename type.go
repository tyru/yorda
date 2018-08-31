package main

import (
	"fmt"
	"strings"
)

// the ID of primitive type
type typeID string

const (
	// NOTE update also "allTypes" in init()!
	typeUnknown typeID = ""
	typeAny     typeID = "tAny"
	typeInt     typeID = "tInt" // In vim script, integer is "Number" but it is ambiguous name.
	typeString  typeID = "tString"
	typeFunc    typeID = "->"
	typeUnion   typeID = "|"
	typeList    typeID = "tList"
	typeDict    typeID = "tDict"
	typeFloat   typeID = "tFloat"
	typeBoolean typeID = "tBoolean"
	typeNone    typeID = "tNone"
	typeJob     typeID = "tJob"
	typeChannel typeID = "tChannel"
	typeTuple   typeID = "tTuple"
)

func init() {
	allTypes := []typeID{
		typeAny,
		typeInt,
		typeString,
		typeFunc,
		typeUnion,
		typeList,
		typeDict,
		typeFloat,
		typeBoolean,
		typeNone,
		typeJob,
		typeChannel,
		typeTuple,
	}

	nameToTypeID = make(map[string]typeID, len(allTypes))
	for _, t := range allTypes {
		id := t.ID()
		if id[0] == 't' {
			nameToTypeID[id[1:]] = t // tInt -> Int
		}
		nameToTypeID[id] = t
	}
}

var nameToTypeID map[string]typeID

func getTypeID(name string) typeID {
	if v, ok := nameToTypeID[name]; ok {
		return v
	}
	return typeUnknown
}

type vimType interface {
	ID() string
}

func (t typeID) ID() string {
	return string(t)
}

type vimTypeVar struct {
	n int
}

func (t *vimTypeVar) ID() string {
	return fmt.Sprintf("T%d", t.n)
}

var typeVarCount int

func newTypeVar() *vimTypeVar {
	typeVarCount++
	return &vimTypeVar{typeVarCount}
}

type composeType struct {
	left  vimType
	right vimType
}

func (t *composeType) ID() string {
	switch t.left {
	case typeTuple:
		list := stringifyTupleType(t)
		if list == "" {
			panic(fmt.Sprintf("not a tuple: %+v", t))
		}
		return fmt.Sprintf("%s(%s)", t.left.ID(), list)
	case typeFunc:
		fun := stringifyFuncType(t)
		if fun == "" {
			panic(fmt.Sprintf("not a function: %+v", t))
		}
		return fmt.Sprintf("(%s)", fun)
	case typeUnion:
		return fmt.Sprintf("(%s)", stringifyUnionType(t))
	default:
		return fmt.Sprintf("(%s) . (%s)", t.left.ID(), t.right.ID())
	}
}

func isTupleType(t vimType) bool {
	return stringifyTupleType(t) != ""
}

func stringifyTupleType(t vimType) string {
	if t == typeTuple { // 0 elements
		return "[]"
	}
	apt, ok := t.(*composeType)
	if !ok || apt.left != typeTuple {
		return ""
	}
	right, ok := apt.right.(*composeType)
	if !ok { // 1 element
		return fmt.Sprintf("[%s]", apt.right.ID())
	}
	apt = right
	list := make([]string, 0, 20)
	for {
		list = append(list, apt.left.ID())
		right, ok := apt.right.(*composeType)
		if !ok {
			list = append(list, apt.right.ID())
			break
		}
		apt = right
	}
	return fmt.Sprintf("[%s]", strings.Join(list, ", "))
}

func stringifyFuncType(t *composeType) string {
	apt := t.right.(*composeType)
	args := stringifyTupleType(apt.left)
	if args == "" {
		return ""
	}
	return fmt.Sprintf("%s -> %s", args, apt.right.ID())
}

func stringifyUnionType(t *composeType) string {
	apt := t.right.(*composeType)
	list := make([]string, 0, 2)
	if apt.left != typeUnion {
		list = append(list, apt.left.ID())
	}
	if right, ok := apt.right.(*composeType); ok {
		list = append(list, stringifyUnionType(right))
	}
	return strings.Join(list, " | ")
}

// newTupleType returns tuple type.
func newTupleType(types ...vimType) vimType {
	switch len(types) {
	case 0:
		return typeTuple
	case 1:
		return &composeType{typeTuple, types[0]}
	case 2:
		a, b := types[0], types[1]
		return &composeType{typeTuple, &composeType{a, b}}
	default:
		a, b, types := types[0], types[1], types[2:]
		return &composeType{typeTuple, newTupleTypeInner(a, b, types...)}
	}
}

func newTupleTypeInner(a, b vimType, types ...vimType) vimType {
	if len(types) == 0 {
		return &composeType{a, b}
	}
	return &composeType{a, newTupleTypeInner(b, types[0], types[1:]...)}
}

// newUnionType returns union type.
func newUnionType(a, b vimType, types ...vimType) vimType {
	if len(types) == 0 {
		return &composeType{typeUnion, &composeType{a, b}}
	}
	return &composeType{typeUnion, &composeType{a, newUnionType(b, types[0], types[1:]...)}}
}

// newFuncType returns function type.
// args is a arguments type (one of tuple types).
// ret is a return type.
// TODO check len(<args types>) <= 20 (Vim script's max number of arguments) ?
func newFuncType(argsType, retType vimType) vimType {
	if !isTupleType(argsType) {
		panic(fmt.Sprintf("newFuncType received invalid arguments type: %+v", argsType))
	}
	return &composeType{typeFunc, &composeType{argsType, retType}}
}
