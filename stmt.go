package main

import (
	"errors"
	"fmt"
	"go/ast"
	"go/token"
	"strings"

	"code.google.com/p/go.tools/go/exact"
	"code.google.com/p/go.tools/go/types"
)

type HasCallVisitor struct {
	info    *types.Info
	hasCall bool
}

func (v *HasCallVisitor) Visit(node ast.Node) (w ast.Visitor) {
	if v.hasCall {
		return nil
	}
	if call, isCall := node.(*ast.CallExpr); isCall {
		if _, isSig := v.info.Types[call.Fun].Type.(*types.Signature); isSig { // skip conversions
			v.hasCall = true
			return nil
		}
	}
	return v
}

func (c *Accum) translateStmtList(stmts []ast.Stmt) {
	for _, stmt := range stmts {
		c.translateStmt(stmt, "")
	}
}

func (c *Accum) translateStmt(stmt ast.Stmt, label string) {
	switch s := stmt.(type) {
	case *ast.BlockStmt:
		c.Printf("(begin \n")
		c.Indent(func() {
			c.translateStmtList(s.List)
		})
		c.Printf("\n)")

	case *ast.ReturnStmt:
		results := s.Results
		if c.resultNames != nil {
			if len(s.Results) != 0 {
				c.translateStmt(&ast.AssignStmt{
					Lhs: c.resultNames,
					Tok: token.ASSIGN,
					Rhs: s.Results,
				}, label)
			}
			results = c.resultNames
		}
		switch len(results) {
		case 0:
			c.Printf("return")
		case 1:
			if c.functionSig.Results().Len() > 1 {
				c.Printf("return(%s)", c.translateExpr(results[0]))
				break
			}
			v := c.translateImplicitConversion(results[0], c.functionSig.Results().At(0).Type())
			c.delayedOutput = nil
			c.Printf("return(%s)", v)
		default:
			values := make([]string, len(results))
			for i, result := range results {
				values[i] = c.translateImplicitConversion(result, c.functionSig.Results().At(i).Type())
			}
			c.delayedOutput = nil
			c.Printf("return(%s)", strings.Join(values, " "))
		}

	case *ast.DeclStmt:
		decl := s.Decl.(*ast.GenDecl)
		switch decl.Tok {
		case token.VAR:
			c.Printf("%s%s;", label, c.translateSimpleStmt(stmt))
		case token.TYPE:
			panic("not yet implemented - jea ") // jea
			/*
				            for _, spec := range decl.Specs {
								o := c.info.Objects[spec.(*ast.TypeSpec).Name].(*types.TypeName)
								c.translateType(o)
								c.initType(o)
							}
			*/
		case token.CONST:
			// skip, constants are inlined
		}

	case *ast.EmptyStmt:
		// skip

	default:
		if r := c.translateSimpleStmt(stmt); r != "" {
			c.Printf("%s%s;", label, r)
		}

	}
}

func (c *Accum) ParseStmt(stmt ast.Stmt, line string) ([]string, error) {
	//fmt.Printf("bird.ParseStmt() called.\n")

	r := make([]string, 0)

	switch s := stmt.(type) {
	case *ast.BlockStmt:
		c.Printf("{\n")
		c.Indent(func() {
			c.translateStmtList(s.List)
		})
		c.Printf("}\n)")

	case *ast.ExprStmt:
		x := stmt.(*ast.ExprStmt)
		tx := c.translateExpr(x.X)
		if c.err != nil {
			return r, c.err
		}
		r = append(r, tx)
		return r, nil

	case *ast.AssignStmt:
		//fmt.Printf("bird: *ast.AssignStmt found.\n")
		e := stmt.(*ast.AssignStmt)
		lhs := e.Lhs
		rhs := e.Rhs
		if len(lhs) < 1 {
			return r, errors.New("no left hand side of assignment")
		}
		if len(rhs) < 1 {
			return r, errors.New("no right hand side of assignment")
		}
		lhsScm := c.TranslateExprSlice(lhs)
		if c.err != nil {
			return r, c.err
		}

		rhsScm := c.TranslateExprSlice(rhs)
		if c.err != nil {
			return r, c.err
		}
		if len(lhsScm) != len(rhsScm) {
			return r, errors.New(fmt.Sprintf("syntax err in '%s': left hand side had %d elements, "+
				"but right-hand side had %d", line, len(lhsScm), len(rhsScm)))
		}
		for i := range lhsScm {
			r = append(r, lhsScm[i]+" = "+rhsScm[i]+";")
		}

		return r, nil
	}

	return r, nil
}

func (c *Accum) translateSimpleStmt(stmt ast.Stmt) string {
	switch s := stmt.(type) {
	case *ast.AssignStmt:
		if s.Tok != token.ASSIGN && s.Tok != token.DEFINE {
			var op token.Token
			switch s.Tok {
			case token.ADD_ASSIGN:
				op = token.ADD
			case token.SUB_ASSIGN:
				op = token.SUB
			case token.MUL_ASSIGN:
				op = token.MUL
			case token.QUO_ASSIGN:
				op = token.QUO
			case token.REM_ASSIGN:
				op = token.REM
			case token.AND_ASSIGN:
				op = token.AND
			case token.OR_ASSIGN:
				op = token.OR
			case token.XOR_ASSIGN:
				op = token.XOR
			case token.SHL_ASSIGN:
				op = token.SHL
			case token.SHR_ASSIGN:
				op = token.SHR
			case token.AND_NOT_ASSIGN:
				op = token.AND_NOT
			default:
				panic(s.Tok)
			}

			var parts []string
			lhs := s.Lhs[0]
			switch l := lhs.(type) {
			case *ast.IndexExpr:
				lhsVar := c.newVariable("_lhs")
				indexVar := c.newVariable("_index")
				parts = append(parts, lhsVar+" = "+c.translateExpr(l.X))
				parts = append(parts, indexVar+" = "+c.translateExpr(l.Index))
				lhs = &ast.IndexExpr{
					X:     c.newIdent(lhsVar, c.info.Types[l.X].Type),
					Index: c.newIdent(indexVar, c.info.Types[l.Index].Type),
				}
				c.info.Types[lhs] = c.info.Types[l]
			case *ast.StarExpr:
				lhsVar := c.newVariable("_lhs")
				parts = append(parts, lhsVar+" = "+c.translateExpr(l.X))
				lhs = &ast.StarExpr{
					X: c.newIdent(lhsVar, c.info.Types[l.X].Type),
				}
				c.info.Types[lhs] = c.info.Types[l]
			case *ast.SelectorExpr:
				v := HasCallVisitor{c.info, false}
				ast.Walk(&v, l.X)
				if v.hasCall {
					lhsVar := c.newVariable("_lhs")
					parts = append(parts, lhsVar+" = "+c.translateExpr(l.X))
					lhs = &ast.SelectorExpr{
						X:   c.newIdent(lhsVar, c.info.Types[l.X].Type),
						Sel: l.Sel,
					}
					c.info.Types[lhs] = c.info.Types[l]
					c.info.Selections[lhs.(*ast.SelectorExpr)] = c.info.Selections[l]
				}
			}

			parenExpr := &ast.ParenExpr{X: s.Rhs[0]}
			c.info.Types[parenExpr] = c.info.Types[s.Rhs[0]]
			binaryExpr := &ast.BinaryExpr{
				X:  lhs,
				Op: op,
				Y:  parenExpr,
			}
			c.info.Types[binaryExpr] = c.info.Types[s.Lhs[0]]
			parts = append(parts, c.translateAssign(lhs, c.translateExpr(binaryExpr)))
			return strings.Join(parts, ", ")
		}

		if s.Tok == token.DEFINE {
			for _, lhs := range s.Lhs {
				if !isBlank(lhs) {
					c.info.Types[lhs] = types.TypeAndValue{Type: c.info.Objects[lhs.(*ast.Ident)].Type()}
				}
			}
		}

		removeParens := func(e ast.Expr) ast.Expr {
			for {
				if p, isParen := e.(*ast.ParenExpr); isParen {
					e = p.X
					continue
				}
				break
			}
			return e
		}

		switch {
		case len(s.Lhs) == 1 && len(s.Rhs) == 1:
			lhs := removeParens(s.Lhs[0])
			if isBlank(lhs) {
				v := HasCallVisitor{c.info, false}
				ast.Walk(&v, s.Rhs[0])
				if v.hasCall {
					return c.translateExpr(s.Rhs[0])
				}
				return ""
			}
			return c.translateAssign(lhs, c.translateImplicitConversion(s.Rhs[0], c.info.Types[s.Lhs[0]].Type))

		case len(s.Lhs) > 1 && len(s.Rhs) == 1:
			tupleVar := c.newVariable("_tuple")
			out := tupleVar + " = " + c.translateExpr(s.Rhs[0])
			tuple := c.info.Types[s.Rhs[0]].Type.(*types.Tuple)
			for i, lhs := range s.Lhs {
				lhs = removeParens(lhs)
				if !isBlank(lhs) {
					out += ", " + c.translateAssign(lhs, c.translateImplicitConversion(c.newIdent(fmt.Sprintf("%s[%d]", tupleVar, i), tuple.At(i).Type()), c.info.Types[s.Lhs[i]].Type))
				}
			}
			return out
		case len(s.Lhs) == len(s.Rhs):
			parts := make([]string, len(s.Rhs))
			for i, rhs := range s.Rhs {
				parts[i] = c.translateImplicitConversion(rhs, c.info.Types[s.Lhs[i]].Type)
			}
			tupleVar := c.newVariable("_tuple")
			out := tupleVar + " = [" + strings.Join(parts, ", ") + "]"
			for i, lhs := range s.Lhs {
				lhs = removeParens(lhs)
				if !isBlank(lhs) {
					out += ", " + c.translateAssign(lhs, fmt.Sprintf("%s[%d]", tupleVar, i))
				}
			}
			return out

		default:
			panic("Invalid arity of AssignStmt.")

		}

	case *ast.IncDecStmt:
		t := c.info.Types[s.X].Type
		if iExpr, isIExpr := s.X.(*ast.IndexExpr); isIExpr {
			switch u := c.info.Types[iExpr.X].Type.Underlying().(type) {
			case *types.Array:
				t = u.Elem()
			case *types.Slice:
				t = u.Elem()
			case *types.Map:
				t = u.Elem()
			}
		}

		tok := token.ADD_ASSIGN
		if s.Tok == token.DEC {
			tok = token.SUB_ASSIGN
		}
		one := &ast.BasicLit{
			Kind:  token.INT,
			Value: "1",
		}
		c.info.Types[one] = types.TypeAndValue{Type: t, Value: exact.MakeInt64(1)}
		return c.translateSimpleStmt(&ast.AssignStmt{
			Lhs: []ast.Expr{s.X},
			Tok: tok,
			Rhs: []ast.Expr{one},
		})

	case *ast.ExprStmt:
		return c.translateExpr(s.X)

	case *ast.DeclStmt:
		var parts []string
		for _, spec := range s.Decl.(*ast.GenDecl).Specs {
			for _, singleSpec := range c.splitValueSpec(spec.(*ast.ValueSpec)) {
				lhs := make([]ast.Expr, len(singleSpec.Names))
				for i, name := range singleSpec.Names {
					lhs[i] = name
				}
				parts = append(parts, c.translateSimpleStmt(&ast.AssignStmt{
					Lhs: lhs,
					Tok: token.DEFINE,
					Rhs: singleSpec.Values,
				}))
			}
		}
		return strings.Join(parts, ", ")

	case *ast.SendStmt:
		return `go$notSupported("send")`

	default:
		panic(fmt.Sprintf("Unhandled statement: %T\n", s))

	}
}

func (c *Accum) translateAssign(lhs ast.Expr, rhs string) string {
	if isBlank(lhs) {
		panic("translateAssign with blank lhs")
	}

	for {
		if p, isParenExpr := lhs.(*ast.ParenExpr); isParenExpr {
			lhs = p.X
			continue
		}
		break
	}

	switch l := lhs.(type) {
	case *ast.Ident:
		return c.objectName(c.info.Objects[l]) + " = " + rhs
	case *ast.SelectorExpr:
		sel := c.info.Selections[l]
		switch sel.Kind() {
		case types.FieldVal:
			fields, jsTag := c.translateSelection(sel)
			if jsTag != "" {
				return fmt.Sprintf("%s.%s.%s = %s", c.translateExpr(l.X), strings.Join(fields, "."), jsTag, c.externalize(rhs, sel.Type()))
			}
			return fmt.Sprintf("%s.%s = %s", c.translateExpr(l.X), strings.Join(fields, "."), rhs)
			return ""
		case types.PackageObj:
			return c.translateExpr(l.X) + "." + l.Sel.Name + " = " + rhs
		default:
			panic(int(sel.Kind()))
		}
	case *ast.StarExpr:
		switch u := c.info.Types[lhs].Type.Underlying().(type) {
		case *types.Struct:
			lVar := c.newVariable("l")
			rVar := c.newVariable("r")
			out := fmt.Sprintf("%s = %s, %s = %s", lVar, c.translateExpr(l.X), rVar, rhs)
			for i := 0; i < u.NumFields(); i++ {
				name := fieldName(u, i)
				out += fmt.Sprintf(", %s.%s = %s.%s", lVar, name, rVar, name)
			}
			return out
		case *types.Array:
			return fmt.Sprintf("go$copyArray(%s, %s)", c.translateExpr(l.X), rhs)
		default:
			return fmt.Sprintf("%s.go$set(%s)", c.translateExpr(l.X), rhs)
		}
	case *ast.IndexExpr:
		switch t := c.info.Types[l.X].Type.Underlying().(type) {
		case *types.Array, *types.Pointer:
			return fmt.Sprintf("%s[%s] = %s", c.translateExpr(l.X), c.flatten64(l.Index), rhs)
		case *types.Slice:
			sliceVar := c.newVariable("_slice")
			indexVar := c.newVariable("_index")
			return fmt.Sprintf("%s = %s, %s = %s", sliceVar, c.translateExpr(l.X), indexVar, c.flatten64(l.Index)) +
				fmt.Sprintf(`, (%s >= 0 && %s < %s.length) ? (%s.array[%s.offset + %s] = %s) : go$throwRuntimeError("index out of range")`, indexVar, indexVar, sliceVar, sliceVar, sliceVar, indexVar, rhs)
		case *types.Map:
			keyVar := c.newVariable("_key")
			return fmt.Sprintf(`%s = %s, (%s || go$throwRuntimeError("assignment to entry in nil map"))[%s] = { k: %s, v: %s }`, keyVar, c.translateImplicitConversion(l.Index, t.Key()), c.translateExpr(l.X), c.makeKey(c.newIdent(keyVar, t.Key()), t.Key()), keyVar, rhs)
		default:
			panic(fmt.Sprintf("Unhandled lhs type: %T\n", t))
		}
	default:
		panic(fmt.Sprintf("Unhandled lhs type: %T\n", l))
	}
}
