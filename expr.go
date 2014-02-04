package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/token"
	"regexp"
	"strconv"
	"strings"

	"code.google.com/p/go.tools/go/exact"
	"code.google.com/p/go.tools/go/types"
)

func (c *Accum) TranslateExprSlice(e []ast.Expr) []string {
	r := make([]string, len(e))
	for i, v := range e {
		r[i] = c.translateExpr(v)
	}
	return r
}

func (c *Accum) translateExpr(expr ast.Expr) string {
	//fmt.Printf("translateExpr called with expr:\n")
	//goon.Dump(expr)

	switch e := expr.(type) {
	case *ast.Ident:
		id := expr.(*ast.Ident)
		//fmt.Printf("id.Name is expr.go is: %#v\n", id.Name)
		if id.Name == "true" {
			return "TRUE"
		}
		if id.Name == "false" {
			return "FALSE"
		}
		return id.Name

	case *ast.BasicLit:
		lit := expr.(*ast.BasicLit)

		//fmt.Printf("lit in BasicLit is: %#v\n", lit)
		switch lit.Kind {
		case token.STRING:
			//fmt.Printf(" we have a token.STRING.\n")
			le := len(lit.Value)
			if lit.Value[0] == '`' && lit.Value[le-1] == '`' {
				//fmt.Printf(" we have a string enclosed in backticks.\n")

				return strconv.Quote(lit.Value[1:(le - 1)])
			}
		}
		return lit.Value

	case *ast.CallExpr:
		callExpr := expr.(*ast.CallExpr)

		switch fun := callExpr.Fun.(type) {
		case *ast.Ident:
			panic("unimplimented")
		case *ast.SelectorExpr:
			x := fun.X.(*ast.Ident)
			pkg := x.Name
			sel := fun.Sel
			name := sel.Name
			args := callExpr.Args
			//fmt.Printf("callExpr pkg:'%s' name:'%s'\n", pkg, name)
			//goon.Dump(callExpr)
			//fmt.Printf("args is\n")
			//goon.Dump(args)
			argSlice := c.TranslateExprSlice(args)
			if c.err != nil {
				return ""
			}
			call := ""
			if pkg == "fmt" && name == "Printf" {
				call = specialCasePrintf(pkg, name, argSlice)
			} else {
				argStr := strings.Join(argSlice, " ")
				call = "(" + pkg + "." + name + " " + argStr + ")"
			}
			return call
		}

		// derived from gopherjs/expressions.go code
	case *ast.UnaryExpr:
		switch e.Op {
		case token.NOT:
			return fmt.Sprintf("!%s", c.translateExpr(e.X))
		case token.SUB:
			return fmt.Sprintf("-%s", c.translateExpr(e.X))
		case token.XOR: // ^5
			// unlikely to be correct, since bitops uses 32-bit integers only.
			return fmt.Sprintf("bitFlip(%s, bitwidth=32)", c.translateExpr(e.X))
		}

	case *ast.BinaryExpr:
		/* // actually implement NEQ below in proper != form
		if e.Op == token.NEQ {
			s := c.translateExpr(&ast.BinaryExpr{
				X:  e.X,
				Op: token.EQL,
				Y:  e.Y,
			})
			return fmt.Sprintf("(not %s)", s)
		}
		*/
		t := c.info.Types[e.X].Type
		t2 := c.info.Types[e.Y].Type

		//fmt.Printf("t is\n")
		//goon.Dump(t)
		//		fmt.Printf("t2 is\n")
		//		goon.Dump(t2)

		_, isInterface := t2.Underlying().(*types.Interface)
		if isInterface {
			t = t2
		}

		// Notes, operators from token.go
		/*
			ADD // +
			SUB // -
			MUL // *
			QUO // /
			REM // %

			AND     // &
			OR      // |
			XOR     // ^
			SHL     // <<
			SHR     // >>
			AND_NOT // &^

			ADD_ASSIGN // +=
			SUB_ASSIGN // -=
			MUL_ASSIGN // *=
			QUO_ASSIGN // /=
			REM_ASSIGN // %=

			AND_ASSIGN     // &=
			OR_ASSIGN      // |=
			XOR_ASSIGN     // ^=
			SHL_ASSIGN     // <<=
			SHR_ASSIGN     // >>=
			AND_NOT_ASSIGN // &^=

			LAND  // &&
			LOR   // ||
			ARROW // <-
			INC   // ++
			DEC   // --

			EQL    // ==
			LSS    // <
			GTR    // >
			ASSIGN // =
			NOT    // !

			NEQ      // !=
			LEQ      // <=
			GEQ      // >=
			DEFINE   // :=
		*/

		basic, isBasic := t.Underlying().(*types.Basic)
		bi := basic.Info()
		isNum := bi & types.IsNumeric
		if isBasic && isNum == 0 {
			//fmt.Printf("we have a non-numeric operation!\n")
			//fmt.Printf("e is %#v\n", e)

			switch e.Op {
			// is NOT a binop or unop to begin with???
			//			case token.NOT:
			// alt: lognot
			//				return fmt.Sprintf("(lognot %s %s)", c.translateExpr(e.X), c.translateExpr(e.Y))

			// see go/src/pkg/go/token/token.go for enum definition of token. types.
			case token.LAND:
				return fmt.Sprintf("%s && %s", c.translateExpr(e.X), c.translateExpr(e.Y))

			case token.LOR:
				return fmt.Sprintf("%s || %s", c.translateExpr(e.X), c.translateExpr(e.Y))

			case token.QUO:
				return fmt.Sprintf("%s / %s", c.translateExpr(e.X), c.translateExpr(e.Y))

			default:
				panic(e.Op)
			}
		}
		if isBasic && isNum != 0 {
			if is64Bit(basic) {
				// integer operations (see below for floating-point ops)

				//fmt.Printf("e is %#v\n", e)
				switch e.Op {
				case token.SHL:
					// require(bitops)
					return fmt.Sprintf("bitShiftL(%s, %s)", c.translateExpr(e.X), c.translateExpr(e.Y))
				case token.SHR:
					// require(bitops)
					return fmt.Sprintf("bitShiftR(%s, %s)", c.translateExpr(e.X), c.translateExpr(e.Y))

				case token.NEQ:
					return fmt.Sprintf("%s != %s", c.translateExpr(e.X), c.translateExpr(e.Y))
				case token.EQL:
					return fmt.Sprintf("%s == %s", c.translateExpr(e.X), c.translateExpr(e.Y))
				case token.MUL, token.LSS, token.LEQ, token.GTR, token.GEQ, token.ADD, token.SUB:
					return fmt.Sprintf("%s %s %s", c.translateExpr(e.X), e.Op.String(), c.translateExpr(e.Y))
				case token.QUO:
					/*
						 in Golang:
							//Q(x, y)   x = q*y + r
							// x   y      r   q
							Q(-5, -2) // -1,  2;  -5 == 2*-2 + -1, checks ok.
							Q(-5, 2)  // -1, -2;  -5 == -2*2 + -1, checks ok.
							Q(5, -2)  //  1, -2;   5 == -2*-2 + 1, checks ok.
							Q(5, 2)   //  1,  2;   5 == 2*2   + 1, checks ok.
					*/

					/*
					   in Chicken scheme:
					   #;27> (quotient -5 -2)
					   2
					   #;28> (quotient -5 2)
					   -2
					   #;29> (quotient 5 -2)
					   -2
					   #;30> (quotient 5 2)
					   2

					*/
					return fmt.Sprintf("quotient(%s, %s)", c.translateExpr(e.X), c.translateExpr(e.Y))

				case token.REM:
					// NB Use remainder, not modulo. The modulo operator doesn't satify the % property.
					//        reference: http://golang.org/ref/spec, Arithmetic operators section.
					//
					// e.g.:      -5 % 2 == -1 in go. aside: -5 / 2 == 2
					// since     (remainder -5 2) == -1
					//    so remainder works;
					// but:      (modulo    -5 2) ==  1
					//    so modulo does not.
					//
					/*
					   // in chicken scheme:
					   #;31> (remainder -5 -2)
					   -1
					   #;32> (remainder -5 2)
					   -1
					   #;33> (remainder 5 -2)
					   1
					   #;34> (remainder 5 2)
					   1
					*/
					return fmt.Sprintf("remainder(%s, %s)", c.translateExpr(e.X), c.translateExpr(e.Y))

				case token.NOT:
					panic("NOT isn't a binop!")
					//return fmt.Sprintf("(bitwise-not %s %s)", c.translateExpr(e.X), c.translateExpr(e.Y))

				case token.AND:
					// alt: logand
					//return fmt.Sprintf("(and %s %s)", c.translateExpr(e.X), c.translateExpr(e.Y))
					return fmt.Sprintf("bitAnd(%s, %s)", c.translateExpr(e.X), c.translateExpr(e.Y))

				case token.OR:
					// alt: logior
					return fmt.Sprintf("bitOr(%s, %s)", c.translateExpr(e.X), c.translateExpr(e.Y))
				case token.XOR:
					// alt: logxor
					return fmt.Sprintf("bitXor(%s, %s)", c.translateExpr(e.X), c.translateExpr(e.Y))

				case token.AND_NOT: // bitwise clear
					return fmt.Sprintf("bitAnd(%s, bitFlip(%s, bitwidth=32))", c.translateExpr(e.X), c.translateExpr(e.Y))
				default:
					panic(e.Op)
				}
			} else {
				// floating point op
				switch e.Op {
				case token.QUO:
					return fmt.Sprintf("%s / %s", c.translateExpr(e.X), c.translateExpr(e.Y))
				case token.EQL:
					return fmt.Sprintf("%s == %s", c.translateExpr(e.X), c.translateExpr(e.Y))
				case token.MUL, token.LSS, token.LEQ, token.GTR, token.GEQ, token.ADD, token.SUB:
					return fmt.Sprintf("%s %s %s", c.translateExpr(e.X), e.Op.String(), c.translateExpr(e.Y))

				}
			}
		}

	}

	return ""
}

func (c *Accum) flatten64(expr ast.Expr) string {
	if is64Bit(c.info.Types[expr].Type.Underlying().(*types.Basic)) {
		return fmt.Sprintf("go$flatten64(%s)", c.translateExpr(expr))
	}
	return c.translateExpr(expr)
}

func toJavaScriptType(t *types.Basic) string {
	switch t.Kind() {
	case types.UntypedInt:
		return "Int"
	case types.Byte:
		return "Uint8"
	case types.Rune:
		return "Int32"
	case types.UnsafePointer:
		return "UnsafePointer"
	default:
		name := t.String()
		return strings.ToUpper(name[:1]) + name[1:]
	}
}

func (c *Accum) formatExpr(format string, a ...interface{}) string {
	var vars = make([]string, len(a))
	var assignments []string
	varFor := func(i int) string {
		v := vars[i]
		if v != "" {
			return v
		}
		e := a[i].(ast.Expr)
		if ident, isIdent := e.(*ast.Ident); isIdent {
			v = c.translateExpr(ident)
			vars[i] = v
			return v
		}
		v = c.newVariable("x")
		assignments = append(assignments, v+" = "+c.translateExpr(e))
		vars[i] = v
		return v
	}
	out := bytes.NewBuffer(nil)
	for i := 0; i < len(format); i++ {
		b := format[i]
		if b == '%' {
			n := int(format[i+1] - '0' - 1)
			k := format[i+2]
			i += 2
			switch k {
			case 's':
				out.WriteString(a[n].(string))
			case 'e':
				out.WriteString(varFor(n))
			case 'h':
				if val := c.info.Types[a[n].(ast.Expr)].Value; val != nil {
					d, _ := exact.Uint64Val(val)
					if c.info.Types[a[n].(ast.Expr)].Type.Underlying().(*types.Basic).Kind() == types.Int64 {
						out.WriteString(strconv.FormatInt(int64(d)>>32, 10))
						continue
					}
					out.WriteString(strconv.FormatUint(d>>32, 10))
					continue
				}
				out.WriteString(varFor(n) + ".high")
			case 'l':
				if val := c.info.Types[a[n].(ast.Expr)].Value; val != nil {
					d, _ := exact.Uint64Val(val)
					out.WriteString(strconv.FormatUint(d&(1<<32-1), 10))
					continue
				}
				out.WriteString(varFor(n) + ".low")
			case 'r':
				if val := c.info.Types[a[n].(ast.Expr)].Value; val != nil {
					r, _ := exact.Float64Val(exact.Real(val))
					out.WriteString(strconv.FormatFloat(r, 'g', -1, 64))
					continue
				}
				out.WriteString(varFor(n) + ".real")
			case 'i':
				if val := c.info.Types[a[n].(ast.Expr)].Value; val != nil {
					i, _ := exact.Float64Val(exact.Imag(val))
					out.WriteString(strconv.FormatFloat(i, 'g', -1, 64))
					continue
				}
				out.WriteString(varFor(n) + ".imag")
			default:
				panic("formatExpr: " + format[i:i+3])
			}
			continue
		}
		out.WriteByte(b)
	}
	if len(assignments) == 0 {
		return out.String()
	}
	return "(" + strings.Join(assignments, ", ") + ", " + out.String() + ")"
}

func (c *Accum) typeName(ty types.Type) string {
	switch t := ty.(type) {
	case *types.Basic:
		switch t.Kind() {
		case types.UntypedNil:
			return "null"
		case types.UnsafePointer:
			return "Go$UnsafePointer"
		default:
			return "Go$" + toJavaScriptType(t)
		}
	case *types.Named:
		if t.Obj().Name() == "error" {
			return "go$error"
		}
		return c.objectName(t.Obj())
	case *types.Pointer:
		return fmt.Sprintf("(go$ptrType(%s))", c.initArgs(t))
	case *types.Interface:
		if t.Empty() {
			return "go$emptyInterface"
		}
		return fmt.Sprintf("(go$interfaceType(%s))", c.initArgs(t))
	case *types.Array, *types.Chan, *types.Slice, *types.Map, *types.Signature, *types.Struct:
		return fmt.Sprintf("(go$%sType(%s))", strings.ToLower(typeKind(t)), c.initArgs(t))
	default:
		panic(fmt.Sprintf("Unhandled type: %T\n", t))
	}
}

func (c *Accum) newVariable(name string) string {
	if name == "" {
		panic("newVariable: empty name")
	}
	for _, b := range []byte(name) {
		if b < '0' || b > 'z' {
			name = "nonAsciiName"
			break
		}
	}
	if strings.HasPrefix(name, "dollar_") {
		name = "$" + name[7:]
	}
	n := c.allVarNames[name]
	c.allVarNames[name] = n + 1
	if n > 0 {
		name = fmt.Sprintf("%s$%d", name, n)
	}
	c.funcVarNames = append(c.funcVarNames, name)
	return name
}

func (c *Accum) objectName(o types.Object) string {
	if o.Pkg() != nil && o.Pkg() != c.pkg {
		pkgVar, found := c.pkgVars[o.Pkg().Path()]
		if !found {
			pkgVar = fmt.Sprintf(`go$packages["%s"]`, o.Pkg().Path())
		}
		return pkgVar + "." + o.Name()
	}

	name, found := c.objectVars[o]
	if !found {
		name = c.newVariable(o.Name())
		c.objectVars[o] = name
	}

	switch o.(type) {
	case *types.Var, *types.Const:
		if o.Exported() && o.Parent() == c.pkg.Scope() {
			return "go$pkg." + name
		}
	}
	return name
}

func typeKind(ty types.Type) string {
	switch t := ty.Underlying().(type) {
	case *types.Basic:
		return toJavaScriptType(t)
	case *types.Array:
		return "Array"
	case *types.Chan:
		return "Chan"
	case *types.Interface:
		return "Interface"
	case *types.Map:
		return "Map"
	case *types.Signature:
		return "Func"
	case *types.Slice:
		return "Slice"
	case *types.Struct:
		return "Struct"
	case *types.Pointer:
		return "Ptr"
	default:
		panic(fmt.Sprintf("Unhandled type: %T\n", t))
	}
}

var FmtRegex = regexp.MustCompile(`[%][^%]*?[^%vTtbcdoqxXUeEfgGsp]*[vTtbcdoqxXUeEfgGsp]`)

func specialCasePrintf(pkg string, name string, argSlice []string) string {
	format := argSlice[0]
	if len(argSlice) > 1 {
		interleaved := interleaveFormatParts(format, argSlice[1:])
		argStr := strings.Join(interleaved, `, `)
		return `cat(paste(sep="", ` + argStr + "))"
	}
	return `cat(` + format + `)`
}

func interleaveFormatParts(format string, argSlice []string) []string {
	//format := `-1%v%v0123%#+v45%v6%%%v`
	format = dequote(format)
	matches := FmtRegex.FindAllStringIndex(format, -1)
	nextArgVal := ""          // always valid
	nextArgI := len(argSlice) // invalid to begin
	if len(argSlice) > 0 {
		nextArgVal = argSlice[0]
		nextArgI = 1 // might be valid, or might not
	}

	//fmt.Printf("format is '%s'\n", format)
	parts := []string{}
	from := 0
	for _, m := range matches {
		//fmt.Printf("k=%d-th match: %v    and from: %v\n", k, m, from)
		if from != m[0] {
			part := format[from:m[0]]
			parts = append(parts, `"`+part+`"`)
		}
		//parts = append(parts, "%")
		parts = append(parts, nextArgVal)
		if nextArgI < len(argSlice) {
			nextArgVal = argSlice[nextArgI]
			nextArgI++
		}

		from = m[1]
	}
	if from <= len(format)-1 {
		parts = append(parts, `"`+format[from:]+`"`)
	}
	//fmt.Printf("at end, from: %d, len(format):%v    parts: %v\n", from, len(format), parts)

	return parts
}

func fixNumber(value string, basic *types.Basic) string {
	switch basic.Kind() {
	case types.Int8:
		return "(" + value + " << 24 >> 24)"
	case types.Uint8:
		return "(" + value + " << 24 >>> 24)"
	case types.Int16:
		return "(" + value + " << 16 >> 16)"
	case types.Uint16:
		return "(" + value + " << 16 >>> 16)"
	case types.Int32, types.Int:
		return "(" + value + " >> 0)"
	case types.Uint32, types.Uint, types.Uintptr:
		return "(" + value + " >>> 0)"
	default:
		panic(int(basic.Kind()))
	}
}

type HasDeferVisitor struct {
	hasDefer bool
}

func (v *HasDeferVisitor) Visit(node ast.Node) (w ast.Visitor) {
	if v.hasDefer {
		return nil
	}
	switch node.(type) {
	case *ast.DeferStmt:
		v.hasDefer = true
		return nil
	case *ast.FuncLit:
		return nil
	}
	return v
}

func (c *Accum) translateImplicitConversion(expr ast.Expr, desiredType types.Type) string {
	if desiredType == nil {
		return c.translateExpr(expr)
	}
	if expr == nil {
		return c.zeroValue(desiredType)
	}

	switch desiredType.Underlying().(type) {
	case *types.Struct, *types.Array:
		if _, isComposite := expr.(*ast.CompositeLit); !isComposite {
			return c.clone(c.translateExpr(expr), desiredType)
		}
	}

	exprType := c.info.Types[expr].Type
	if types.Identical(exprType, desiredType) {
		return c.translateExpr(expr)
	}

	basicExprType, isBasicExpr := exprType.Underlying().(*types.Basic)
	if isBasicExpr && basicExprType.Kind() == types.UntypedNil {
		return c.zeroValue(desiredType)
	}

	switch desiredType.Underlying().(type) {
	case *types.Slice:
		return c.formatExpr("go$subslice(new %1s(%2e.array), %2e.offset, %2e.offset + %2e.length)", c.typeName(desiredType), expr)

	case *types.Interface:
		if isWrapped(exprType) {
			return fmt.Sprintf("new %s(%s)", c.typeName(exprType), c.translateExpr(expr))
		}
		if _, isStruct := exprType.Underlying().(*types.Struct); isStruct {
			return c.formatExpr("new %1e.constructor.Struct(%1e)", expr)
		}
	}

	return c.translateExpr(expr)
}

func (c *Accum) clone(src string, ty types.Type) string {
	switch t := ty.Underlying().(type) {
	case *types.Struct:
		structVar := c.newVariable("_struct")
		fields := make([]string, t.NumFields())
		for i := range fields {
			fields[i] = c.clone(structVar+"."+fieldName(t, i), t.Field(i).Type())
		}
		constructor := structVar + ".constructor"
		if named, isNamed := ty.(*types.Named); isNamed {
			constructor = c.objectName(named.Obj()) + ".Ptr"
		}
		return fmt.Sprintf("(%s = %s, new %s(%s))", structVar, src, constructor, strings.Join(fields, ", "))
	case *types.Array:
		return fmt.Sprintf("go$mapArray(%s, function(entry) { return %s; })", src, c.clone("entry", t.Elem()))
	default:
		return src
	}
}

func dequote(s string) string {
	if len(s) < 1 {
		return s
	}
	r := s
	if s[0] == '"' {
		r = s[1:]
	}
	le := len(r)
	if r[le-1] == '"' {
		r = r[:le-1]
	}
	return r
}
