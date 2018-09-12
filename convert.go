// This file contains prolog converter, which converts ast.Node to prolog syntax.
package main

import (
	"bytes"
	"fmt"
	"io"
	"strconv"
	"strings"

	"github.com/haya14busa/go-vimlparser/ast"
	"github.com/haya14busa/go-vimlparser/token"
	"github.com/pkg/errors"
)

type errorReader struct {
	err error
}

func (r *errorReader) Read([]byte) (int, error) {
	return 0, r.err
}

// errorf returns an error which include node position, and error message.
func errorf(node ast.Node, format string, args ...interface{}) error {
	format = "%s: " + format
	newargs := make([]interface{}, 0, len(args)+1)
	newargs = append(newargs, node.Pos().String())
	newargs = append(newargs, args...)
	return errors.Errorf(format, newargs...)
}

type converter struct {
	depth  int    // for indent
	indent string // a string per 1 indent
}

func (c *converter) incIndent() {
	c.depth++
}

func (c *converter) decIndent() {
	c.depth--
}

func (c *converter) getIndent() string {
	return strings.Repeat(c.indent, c.depth)
}

func (c *converter) toReader(node ast.Node) io.Reader {
	var buf bytes.Buffer

	switch n := node.(type) {
	case *ast.File:
		// file([Args...]) @ Pos
		buf.WriteString(`:- consult('../lib/vimscript.pl').
:- use_module('../lib/vimscript.pl').

main :- eval(file([`)
		if len(n.Body) > 0 {
			c.incIndent()
			if err := c.writeStmtList(&buf, n.Body); err != nil {
				return &errorReader{err}
			}
			buf.WriteString(c.getIndent())
			c.decIndent()
		}
		buf.WriteString("])")
		c.writePos(&buf, node)

		buf.WriteString(`, _, _), halt.

:- main; halt(1).
`)
		return &buf

	case *ast.Comment:
		// comment(Text) @ Pos
		buf.WriteString("comment(")
		buf.WriteString(quote(n.Text))
		buf.WriteString(")")

	case *ast.Excmd:
		// excmd(Command) @ Pos
		// TODO dump also n.ExArg ?
		buf.WriteString("excmd(")
		buf.WriteString(quote(n.Command))
		buf.WriteString(")")

	case *ast.Function:
		// function(Name, [Params...], [Body...]) @ Pos
		buf.WriteString("function(")
		if _, err := io.Copy(&buf, c.toReader(n.Name)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(",[")
		if err := c.writeIdentList(&buf, n.Params); err != nil {
			return &errorReader{err}
		}
		buf.WriteString("],[")
		if err := c.writeStmtList(&buf, n.Body); err != nil {
			return &errorReader{err}
		}
		buf.WriteString("])")

	case *ast.EndFunction: // nothing to do

	case *ast.DelFunction:
		// delfunction(Name) @ Pos
		buf.WriteString("delfunction(")
		if _, err := io.Copy(&buf, c.toReader(n.Name)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(")")

	case *ast.Return:
		if n.Result != nil {
			// return(result) @ Pos
			buf.WriteString("return(")
			if _, err := io.Copy(&buf, c.toReader(n.Result)); err != nil {
				return &errorReader{err}
			}
			buf.WriteString(")")
		} else {
			// return() @ Pos
			buf.WriteString("return()")
		}

	case *ast.ExCall:
		// excall(FuncCall) @ Pos
		buf.WriteString("excall(")
		if _, err := io.Copy(&buf, c.toReader(n.FuncCall)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(")")

	case *ast.Let:
		// let(lhs, Op, rhs) @ Pos
		// lhs = [a, b, c | rest]
		buf.WriteString("let(")
		if err := c.writeLHS(&buf, n.Left, n.List, n.Rest); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(",")
		buf.WriteString(n.Op)
		buf.WriteString(",")
		if _, err := io.Copy(&buf, c.toReader(n.Right)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(")")

	case *ast.UnLet:
		// unlet([expr...]) @ Pos
		buf.WriteString("unlet([")
		if err := c.writeExprList(&buf, n.List); err != nil {
			return &errorReader{err}
		}
		buf.WriteString("])")

	case *ast.LockVar:
		// lockvar(Depth, [Expr...]) @ Pos
		buf.WriteString("lockvar(")
		buf.WriteString(strconv.Itoa(n.Depth))
		buf.WriteString(",[")
		if err := c.writeExprList(&buf, n.List); err != nil {
			return &errorReader{err}
		}
		buf.WriteString("])")

	case *ast.UnLockVar:
		// unlockvar(Depth, [Expr...]) @ Pos
		buf.WriteString("unlockvar(")
		buf.WriteString(strconv.Itoa(n.Depth))
		buf.WriteString(",[")
		if err := c.writeExprList(&buf, n.List); err != nil {
			return &errorReader{err}
		}
		buf.WriteString("])")

	case *ast.If:
		// if(cond, [body...]) @ Pos
		// if(cond, [body...], else([body...])) @ Pos
		// if(cond, [body...], elseif([cond, [body...], ...])) @ Pos
		// if(cond, [body...], elseif([cond, [body...], ...]), else([body...])) @ Pos
		buf.WriteString("if(")
		if _, err := io.Copy(&buf, c.toReader(n.Condition)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(",[")
		if err := c.writeStmtList(&buf, n.Body); err != nil {
			return &errorReader{err}
		}
		buf.WriteString("]")
		if len(n.ElseIf) > 0 {
			buf.WriteString(",elseif([")
			for i, elseif := range n.ElseIf {
				if i > 0 {
					buf.WriteString(",")
				}
				if _, err := io.Copy(&buf, c.toReader(elseif.Condition)); err != nil {
					return &errorReader{err}
				}
				buf.WriteString(",[")
				if err := c.writeStmtList(&buf, elseif.Body); err != nil {
					return &errorReader{err}
				}
				buf.WriteString("]")
			}
			buf.WriteString("])")
		}
		if n.Else != nil {
			buf.WriteString(",else([")
			if err := c.writeStmtList(&buf, n.Else.Body); err != nil {
				return &errorReader{err}
			}
			buf.WriteString("])")
		}
		buf.WriteString(")")

	case *ast.ElseIf: // nothing to do

	case *ast.Else: // nothing to do

	case *ast.EndIf: // nothing to do

	case *ast.While:
		// while(Cond, [Body...]) @ Pos
		buf.WriteString("while(")
		if _, err := io.Copy(&buf, c.toReader(n.Condition)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(",[")
		if err := c.writeStmtList(&buf, n.Body); err != nil {
			return &errorReader{err}
		}
		buf.WriteString("])")

	case *ast.EndWhile: // nothing to do

	case *ast.For:
		// for(Lhs, in, Rhs, [Body...]) @ Pos
		buf.WriteString("for(")
		if err := c.writeLHS(&buf, n.Left, n.List, n.Rest); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(",in,")
		if _, err := io.Copy(&buf, c.toReader(n.Right)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(",[")
		if err := c.writeStmtList(&buf, n.Body); err != nil {
			return &errorReader{err}
		}
		buf.WriteString("])")

	case *ast.EndFor: // nothing to do

	case *ast.Continue: // nothing to do
		// continue() @ Pos
		buf.WriteString("continue()")

	case *ast.Break: // nothing to do
		// break() @ Pos
		buf.WriteString("break()")

	case *ast.Try:
		// try([body...]) @ Pos
		// try([body...], catch([Pattern, [body...], ...])) @ Pos
		// try([body...], catch([Pattern, [body...], ...]), finally([body...])) @ Pos
		buf.WriteString("try([")
		if err := c.writeStmtList(&buf, n.Body); err != nil {
			return &errorReader{err}
		}
		buf.WriteString("]")
		if len(n.Catch) > 0 {
			buf.WriteString(",catch([")
			for i, catch := range n.Catch {
				if i > 0 {
					buf.WriteString(",")
				}
				buf.WriteString(quote(catch.Pattern))
				buf.WriteString(",[")
				if err := c.writeStmtList(&buf, catch.Body); err != nil {
					return &errorReader{err}
				}
				buf.WriteString("]")
			}
			buf.WriteString("])")
		}
		if n.Finally != nil {
			buf.WriteString(",finally([")
			if err := c.writeStmtList(&buf, n.Finally.Body); err != nil {
				return &errorReader{err}
			}
			buf.WriteString("])")
		}
		buf.WriteString(")")

	case *ast.Catch: // nothing to do

	case *ast.Finally: // nothing to do

	case *ast.EndTry: // nothing to do

	case *ast.Throw:
		// throw(Expr) @ Pos
		buf.WriteString("throw(")
		if _, err := io.Copy(&buf, c.toReader(n.Expr)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(")")

	case *ast.EchoCmd:
		// echo([Expr...]) @ Pos
		// echon([Expr...]) @ Pos
		// echomsg([Expr...]) @ Pos
		// echoerr([Expr...]) @ Pos
		buf.WriteString(n.CmdName)
		buf.WriteString("([")
		if err := c.writeExprList(&buf, n.Exprs); err != nil {
			return &errorReader{err}
		}
		buf.WriteString("])")

	case *ast.Echohl:
		// echohl(Name) @ Pos
		buf.WriteString("echohl(")
		buf.WriteString(quote(n.Name))
		buf.WriteString(")")

	case *ast.Execute:
		// execute([Expr...]) @ Pos
		buf.WriteString("execute([")
		if err := c.writeExprList(&buf, n.Exprs); err != nil {
			return &errorReader{err}
		}
		buf.WriteString("])")

	case *ast.TernaryExpr:
		// ternary(Cond, Left, Right) @ Pos
		buf.WriteString("ternary(")
		if _, err := io.Copy(&buf, c.toReader(n.Condition)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(",")
		if _, err := io.Copy(&buf, c.toReader(n.Left)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(",")
		if _, err := io.Copy(&buf, c.toReader(n.Right)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(")")

	case *ast.BinaryExpr:
		// op(Left, Op, Right) @ Pos
		buf.WriteString("op(")
		if _, err := io.Copy(&buf, c.toReader(n.Left)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(",")
		buf.WriteString(n.Op.String())
		buf.WriteString(",")
		if _, err := io.Copy(&buf, c.toReader(n.Right)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(")")

	case *ast.UnaryExpr:
		// op(Op, Expr) @ Pos
		buf.WriteString("op(")
		buf.WriteString(n.Op.String())
		buf.WriteString(",")
		if _, err := io.Copy(&buf, c.toReader(n.X)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(")")

	case *ast.SubscriptExpr:
		// subscript(Left, Right) @ Pos
		buf.WriteString("subscript(")
		if _, err := io.Copy(&buf, c.toReader(n.Left)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(",")
		if _, err := io.Copy(&buf, c.toReader(n.Right)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(")")

	case *ast.SliceExpr:
		// slice(X, Low, :, High) @ Pos
		// slice(X, :, High) @ Pos
		// slice(X, Low, :) @ Pos
		// slice(X, :) @ Pos
		buf.WriteString("slice(")
		if _, err := io.Copy(&buf, c.toReader(n.X)); err != nil {
			return &errorReader{err}
		}
		if n.Low != nil {
			buf.WriteString(",")
			if _, err := io.Copy(&buf, c.toReader(n.Low)); err != nil {
				return &errorReader{err}
			}
		}
		buf.WriteString(",:")
		if n.High != nil {
			buf.WriteString(",")
			if _, err := io.Copy(&buf, c.toReader(n.High)); err != nil {
				return &errorReader{err}
			}
		}
		buf.WriteString(")")

	case *ast.CallExpr:
		// call(Fun, [Args...]) @ Pos
		buf.WriteString("call(")
		if _, err := io.Copy(&buf, c.toReader(n.Fun)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(",[")
		if err := c.writeExprList(&buf, n.Args); err != nil {
			return &errorReader{err}
		}
		buf.WriteString("])")

	case *ast.DotExpr:
		// dot(Left, Right) @ Pos
		buf.WriteString("dot(")
		if _, err := io.Copy(&buf, c.toReader(n.Left)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(",")
		if _, err := io.Copy(&buf, c.toReader(n.Right)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(")")

	case *ast.BasicLit:
		switch n.Kind {
		case token.NUMBER:
			// tInt(Value) @ Pos
			buf.WriteString("tInt(")
			buf.WriteString(n.Value)
			buf.WriteString(")")
		case token.STRING:
			// tString(Value) @ Pos
			buf.WriteString("tString(")
			buf.WriteString(toDQString(n.Value))
			buf.WriteString(")")
		case token.OPTION:
			// option(Name) @ Pos
			buf.WriteString("option(")
			buf.WriteString(quote(n.Value))
			buf.WriteString(")")
		case token.ENV:
			// env(Name) @ Pos
			buf.WriteString("env(")
			buf.WriteString(quote(n.Value))
			buf.WriteString(")")
		case token.REG:
			// reg(Name) @ Pos
			buf.WriteString("reg(")
			buf.WriteString(quote(n.Value))
			buf.WriteString(")")
		default:
			err := errorf(n, "token (%s) is not implemented yet", n.Kind.String())
			return &errorReader{err}
		}

	case *ast.List:
		// tList([Values...]) @ Pos
		buf.WriteString("tList([")
		if err := c.writeExprList(&buf, n.Values); err != nil {
			return &errorReader{err}
		}
		buf.WriteString("])")

	case *ast.Dict:
		// tDict([Key, Value, ...]) @ Pos
		buf.WriteString("tDict([")
		for i := range n.Entries {
			if i > 0 {
				buf.WriteString(",")
			}
			e := &n.Entries[i]
			if _, err := io.Copy(&buf, c.toReader(e.Key)); err != nil {
				return &errorReader{err}
			}
			buf.WriteString(",")
			if _, err := io.Copy(&buf, c.toReader(e.Value)); err != nil {
				return &errorReader{err}
			}
		}
		buf.WriteString("])")

	case *ast.Ident:
		// ident(Scope, Name, Lv) @ Pos
		var scope string
		name := n.Name
		if len(n.Name) >= 2 && n.Name[1] == ':' {
			scope = n.Name[:1]
			name = n.Name[2:]
		}
		buf.WriteString("ident(")
		buf.WriteString(quote(scope))
		buf.WriteString(",")
		buf.WriteString(quote(name))
		buf.WriteString(")")

	case *ast.CurlyName:
		// curlyName([curlyNameExpr() or curlyNameLit(), ...]) @ Pos
		buf.WriteString("curlyName([")
		for i, e := range n.Parts {
			if i > 0 {
				buf.WriteString(",")
			}
			if _, err := io.Copy(&buf, c.toReader(e)); err != nil {
				return &errorReader{err}
			}
		}
		buf.WriteString("])")

	case *ast.CurlyNameLit:
		// curlyNameLit(Value) @ Pos
		buf.WriteString("curlyNameLit(")
		buf.WriteString(quote(n.Value))
		buf.WriteString(")")

	case *ast.CurlyNameExpr:
		// curlyNameExpr(Value) @ Pos
		buf.WriteString("curlyNameExpr(")
		if _, err := io.Copy(&buf, c.toReader(n.Value)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(")")

	case *ast.LambdaExpr:
		// lambda([Params...], Expr) @ Pos
		buf.WriteString("lambda([")
		if err := c.writeIdentList(&buf, n.Params); err != nil {
			return &errorReader{err}
		}
		buf.WriteString("],")
		if _, err := io.Copy(&buf, c.toReader(n.Expr)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(")")

	case *ast.ParenExpr:
		// par(Expr) @ Pos
		buf.WriteString("par(")
		if _, err := io.Copy(&buf, c.toReader(n.X)); err != nil {
			return &errorReader{err}
		}
		buf.WriteString(")")

	default:
		panic(fmt.Sprintf("ast.Walk: unexpected node type %T", n))
	}

	c.writePos(&buf, node)

	return &buf
}

func (c *converter) writePos(buf *bytes.Buffer, node ast.Node) {
	pos := node.Pos()
	buf.WriteString(" @ [")
	buf.WriteString(strconv.Itoa(pos.Line))
	buf.WriteString(",")
	buf.WriteString(strconv.Itoa(pos.Column))
	buf.WriteString("]")
}

// writeIdentList writes ident list without "[", "]" .
func (c *converter) writeIdentList(buf *bytes.Buffer, list []*ast.Ident) error {
	for i := range list {
		if i > 0 {
			buf.WriteString(",")
		}
		_, err := io.Copy(buf, c.toReader(list[i]))
		if err != nil {
			return err
		}
	}
	return nil
}

// writeExprList writes expressions without "[", "]" .
func (c *converter) writeExprList(buf *bytes.Buffer, list []ast.Expr) error {
	for i := range list {
		if i > 0 {
			buf.WriteString(",")
		}
		_, err := io.Copy(buf, c.toReader(list[i]))
		if err != nil {
			return err
		}
	}
	return nil
}

// writeStmtList writes expressions without "[", "]" .
func (c *converter) writeStmtList(buf *bytes.Buffer, list []ast.Statement) error {
	if len(list) == 0 {
		return nil
	}
	c.incIndent()
	buf.WriteString("\n")
	for i := range list {
		if i > 0 {
			buf.WriteString(",\n")
		}
		buf.WriteString(c.getIndent())
		_, err := io.Copy(buf, c.toReader(list[i]))
		if err != nil {
			return err
		}
	}
	c.decIndent()
	buf.WriteString("\n")
	buf.WriteString(c.getIndent())
	return nil
}

// quote converts raw string to prolog string.
func quote(s string) string {
	return "\"" + strings.Replace(s, "\"", "\\\"", -1) + "\""
}

// toDQString converts BasicLit.Value string to prolog string.
// len(BasicLit.Value) must be equal or greater than 2.
// Because " or ' is at the begin and the end of the string.
func toDQString(s string) string {
	if s[0] == '"' {
		return s
	}
	return "\"" + strings.Replace(s[1:len(s)-1], "\"", "\\\"", -1) + "\""
}

func (c *converter) writeLHS(buf *bytes.Buffer, left ast.Expr, list []ast.Expr, rest ast.Expr) error {
	if left != nil {
		if _, err := io.Copy(buf, c.toReader(left)); err != nil {
			return err
		}
	} else if list != nil {
		buf.WriteString("[")
		if err := c.writeExprList(buf, list); err != nil {
			return err
		}
		if rest != nil {
			buf.WriteString(";")
			if _, err := io.Copy(buf, c.toReader(rest)); err != nil {
				return err
			}
		}
		buf.WriteString("]")
	}
	return nil
}
