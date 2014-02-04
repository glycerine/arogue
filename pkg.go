package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"strings"

	"code.google.com/p/go.tools/go/types"
)

var ReservedKeywords = make(map[string]bool)

func init() {
	for _, keyword := range []string{"abstract", "arguments", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "debugger", "default", "delete", "do", "double", "else", "enum", "eval", "export", "extends", "false", "final", "finally", "float", "for", "function", "goto", "if", "implements", "import", "in", "instanceof", "int", "interface", "let", "long", "native", "new", "package", "private", "protected", "public", "return", "short", "static", "super", "switch", "synchronized", "this", "throw", "throws", "transient", "true", "try", "typeof", "var", "void", "volatile", "while", "with", "yield"} {
		ReservedKeywords[keyword] = true
	}

	// jea:
	// and the golang reserved words, i.e. from
	// http://golang.org/ref/spec#Keywords
	for _, keyword := range []string{"break",
		"default", "func", "interface", "select",
		"case", "defer", "go", "map", "struct",
		"chan", "else", "goto", "package", "switch",
		"const", "fallthrough", "if", "range", "type",
		"continue", "for", "import", "return", "var"} {
		ReservedKeywords[keyword] = true
	}

}

func (c *Accum) Write(b []byte) (int, error) {
	c.output = append(c.output, b...)
	return len(b), nil
}

func (c *Accum) Printf(format string, values ...interface{}) {
	c.Write([]byte(strings.Repeat("\t", c.indentation)))
	fmt.Fprintf(c, format, values...)
	c.Write([]byte{'\n'})
	c.Write(c.delayedOutput)
	c.delayedOutput = nil
}

func (c *Accum) Indent(f func()) {
	c.indentation += 1
	f()
	c.indentation -= 1
}

func (c *Accum) CatchOutput(f func()) []byte {
	origoutput := c.output
	c.output = nil
	f()
	catched := c.output
	c.output = origoutput
	return catched
}

func (c *Accum) splitValueSpec(s *ast.ValueSpec) []*ast.ValueSpec {
	if len(s.Values) == 1 {
		if _, isTuple := c.info.Types[s.Values[0]].Type.(*types.Tuple); isTuple {
			return []*ast.ValueSpec{s}
		}
	}

	list := make([]*ast.ValueSpec, len(s.Names))
	for i, name := range s.Names {
		var value ast.Expr
		if i < len(s.Values) {
			value = s.Values[i]
		}
		list[i] = &ast.ValueSpec{
			Names:  []*ast.Ident{name},
			Values: []ast.Expr{value},
		}
	}
	return list
}

func (c *Accum) translateFunction(fun *ast.FuncDecl, natives map[string]string, translateNatives bool) {
	//	inline/defer the c.newScope call

	outerVarNames := make(map[string]int, len(c.allVarNames))
	for k, v := range c.allVarNames {
		outerVarNames[k] = v
	}
	outerFuncVarNames := c.funcVarNames
	defer func() {
		c.allVarNames = outerVarNames
		c.funcVarNames = outerFuncVarNames
	}()
	// end c.newScope call manual inline

	sig := c.info.Objects[fun.Name].(*types.Func).Type().(*types.Signature)
	var recv *ast.Ident
	if fun.Recv != nil && fun.Recv.List[0].Names != nil {
		recv = fun.Recv.List[0].Names[0]
	}
	params := c.translateParams(fun.Type)
	joinedParams := strings.Join(params, ", ")

	printPrimaryFunction := func(lhs string, fullName string) bool {
		native, hasNative := natives[fullName]
		if translateNatives != hasNative {
			return false
		}
		if hasNative {
			c.Printf("%s = %s;", lhs, native)
			delete(natives, fullName)
			return true
		}

		c.Printf("%s = function(%s) {", lhs, joinedParams)
		c.Indent(func() {
			if fun.Body == nil {
				c.Printf(`throw go$panic("Native function not implemented: %s");`, fullName)
				return
			}

			body := fun.Body.List
			if recv != nil {
				recvType := sig.Recv().Type()
				c.info.Types[recv] = types.TypeAndValue{Type: recvType}
				this := c.newIdent("this", recvType)
				if isWrapped(recvType) {
					this = c.newIdent("this.go$val", recvType)
				}
				body = append([]ast.Stmt{
					&ast.AssignStmt{
						Lhs: []ast.Expr{recv},
						Tok: token.DEFINE,
						Rhs: []ast.Expr{this},
					},
				}, body...)
			}
			c.translateFunctionBody(body, sig)
		})
		c.Printf("};")
		return true
	}

	if fun.Recv == nil {
		funName := c.objectName(c.info.Objects[fun.Name])
		lhs := "var " + funName
		if fun.Name.IsExported() || fun.Name.Name == "main" {
			lhs += " = go$pkg." + funName
		}
		printPrimaryFunction(lhs, funName)
		return
	}

	recvType := sig.Recv().Type()
	ptr, isPointer := recvType.(*types.Pointer)
	namedRecvType, _ := recvType.(*types.Named)
	if isPointer {
		namedRecvType = ptr.Elem().(*types.Named)
	}
	typeName := c.objectName(namedRecvType.Obj())
	funName := fun.Name.Name
	if ReservedKeywords[funName] {
		funName += "$"
	}

	if _, isStruct := namedRecvType.Underlying().(*types.Struct); isStruct {
		if printPrimaryFunction(typeName+".Ptr.prototype."+funName, typeName+"."+funName) {
			c.Printf("%s.prototype.%s = function(%s) { return this.go$val.%s(%s); };", typeName, funName, joinedParams, funName, joinedParams)
		}
		return
	}

	if isPointer {
		if _, isArray := ptr.Elem().Underlying().(*types.Array); isArray {
			if printPrimaryFunction(typeName+".prototype."+funName, typeName+"."+funName) {
				c.Printf("go$ptrType(%s).prototype.%s = function(%s) { return (new %s(this.go$get())).%s(%s); };", typeName, funName, joinedParams, typeName, funName, joinedParams)
			}
			return
		}
		value := "this"
		if isWrapped(ptr.Elem()) {
			value = "this.go$val"
		}
		if printPrimaryFunction(fmt.Sprintf("go$ptrType(%s).prototype.%s", typeName, funName), typeName+"."+funName) {
			c.Printf("%s.prototype.%s = function(%s) { var obj = %s; return (new (go$ptrType(%s))(function() { return obj; }, null)).%s(%s); };", typeName, funName, joinedParams, value, typeName, funName, joinedParams)
		}
		return
	}

	value := "this.go$get()"
	if isWrapped(recvType) {
		value = fmt.Sprintf("new %s(%s)", typeName, value)
	}
	if printPrimaryFunction(typeName+".prototype."+funName, typeName+"."+funName) {
		c.Printf("go$ptrType(%s).prototype.%s = function(%s) { return %s.%s(%s); };", typeName, funName, joinedParams, value, funName, joinedParams)
	}

}

func (c *Accum) translateFunctionBody(stmts []ast.Stmt, sig *types.Signature) {
	c.funcVarNames = nil

	body := c.CatchOutput(func() {
		var resultNames []ast.Expr
		if sig != nil && sig.Results().Len() != 0 && sig.Results().At(0).Name() != "" {
			resultNames = make([]ast.Expr, sig.Results().Len())
			for i := 0; i < sig.Results().Len(); i++ {
				result := sig.Results().At(i)
				name := result.Name()
				if result.Name() == "_" {
					name = "result"
				}
				id := ast.NewIdent(name)
				c.info.Types[id] = types.TypeAndValue{Type: result.Type()}
				c.info.Objects[id] = result
				c.Printf("%s = %s;", c.translateExpr(id), c.zeroValue(result.Type()))
				resultNames[i] = id
			}
		}

		if sig != nil {
			s := c.functionSig
			defer func() { c.functionSig = s }()
			c.functionSig = sig
		}
		r := c.resultNames
		defer func() { c.resultNames = r }()
		c.resultNames = resultNames
		p := c.postLoopStmt
		defer func() { c.postLoopStmt = p }()
		c.postLoopStmt = make(map[string]ast.Stmt)

		v := HasDeferVisitor{}
		ast.Walk(&v, &ast.BlockStmt{List: stmts})
		switch v.hasDefer {
		case true:
			c.Printf("var go$deferred = [];")
			c.Printf("try {")
			c.Indent(func() {
				c.translateStmtList(stmts)
			})
			c.Printf("} catch(go$err) {")
			c.Indent(func() {
				c.Printf("go$pushErr(go$err);")
				if sig != nil && resultNames == nil {
					switch sig.Results().Len() {
					case 0:
						// nothing
					case 1:
						c.Printf("return %s;", c.zeroValue(sig.Results().At(0).Type()))
					default:
						zeros := make([]string, sig.Results().Len())
						for i := range zeros {
							zeros[i] = c.zeroValue(sig.Results().At(i).Type())
						}
						c.Printf("return [%s];", strings.Join(zeros, ", "))
					}
				}
			})
			c.Printf("} finally {")
			c.Indent(func() {
				c.Printf("go$callDeferred(go$deferred);")
				if resultNames != nil {
					c.translateStmt(&ast.ReturnStmt{}, "")
				}
			})
			c.Printf("}")
		case false:
			c.translateStmtList(stmts)
		}
	})

	if len(c.funcVarNames) != 0 {
		c.Printf("var %s;", strings.Join(c.funcVarNames, ", "))
	}
	c.Write(body)
}

func (c *Accum) initArgs(ty types.Type) string {
	switch t := ty.(type) {
	case *types.Array:
		return fmt.Sprintf("%s, %d", c.typeName(t.Elem()), t.Len())
	case *types.Chan:
		return fmt.Sprintf("%s, %t, %t", c.typeName(t.Elem()), t.Dir()&types.SendOnly != 0, t.Dir()&types.RecvOnly != 0)
	case *types.Interface:
		methods := make([]string, t.NumMethods())
		for i := range methods {
			method := t.Method(i)
			pkgPath := ""
			if !method.Exported() {
				pkgPath = method.Pkg().Path()
			}
			methods[i] = fmt.Sprintf(`["%s", "%s", %s]`, method.Name(), pkgPath, c.typeName(method.Type()))
		}
		return fmt.Sprintf("[%s]", strings.Join(methods, ", "))
	case *types.Map:
		return fmt.Sprintf("%s, %s", c.typeName(t.Key()), c.typeName(t.Elem()))
	case *types.Pointer:
		return fmt.Sprintf("%s", c.typeName(t.Elem()))
	case *types.Slice:
		return fmt.Sprintf("%s", c.typeName(t.Elem()))
	case *types.Signature:
		return fmt.Sprintf("%s, %s, %t", c.typeArray(t.Params()), c.typeArray(t.Results()), t.Variadic())
	case *types.Struct:
		fields := make([]string, t.NumFields())
		for i := range fields {
			field := t.Field(i)
			name := ""
			if !field.Anonymous() {
				name = field.Name()
			}
			pkgPath := ""
			if !field.Exported() {
				pkgPath = field.Pkg().Path()
			}
			fields[i] = fmt.Sprintf(`["%s", "%s", %s, %s]`, name, pkgPath, c.typeName(field.Type()), encodeString(t.Tag(i)))
		}
		return fmt.Sprintf("[%s]", strings.Join(fields, ", "))
	default:
		panic("invalid type")
	}
}

func (c *Accum) typeArray(t *types.Tuple) string {
	s := make([]string, t.Len())
	for i := range s {
		s[i] = c.typeName(t.At(i).Type())
	}
	return "[" + strings.Join(s, ", ") + "]"
}
