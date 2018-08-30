package main

import (
	"github.com/haya14busa/go-vimlparser/ast"
	"github.com/pkg/errors"
)

// errorf returns an error which include node position, and error message.
func errorf(node ast.Node, format string, args ...interface{}) error {
	format = "%s: " + format
	newargs := make([]interface{}, 0, len(args)+1)
	newargs = append(newargs, node.Pos().String())
	newargs = append(newargs, args...)
	return errors.Errorf(format, newargs...)
}
