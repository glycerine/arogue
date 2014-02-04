package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"regexp"
	"strconv"
	"strings"

	"code.google.com/p/go.tools/go/types"
)

var FuncNameRegex = regexp.MustCompile(`^[\t ]*func[\t ]+([^( ]+)[\t ]*[(]`)

func isFuncDefinition(line string) (bool, string) {
	trimmed := strings.TrimLeft(line, "\t ")
	if strings.HasPrefix(trimmed, "func ") {
		name := FuncNameRegex.FindStringSubmatch(line)
		//fmt.Printf("name is %#v\n", name)
		if name == nil || len(name) < 2 {
			return false, ""
		}
		return true, name[1]
	}
	return false, ""
}

func is64Bit(t *types.Basic) bool {
	return t.Kind() == types.Int64 || t.Kind() == types.Uint64 || t.Kind() == types.UntypedInt
}

func isUntypedInt(t *types.Basic) bool {
	return t.Kind() == types.UntypedInt
}

func encodeString(s string) string {
	buffer := bytes.NewBuffer(nil)
	for _, r := range []byte(s) {
		switch r {
		case '\b':
			buffer.WriteString(`\b`)
		case '\f':
			buffer.WriteString(`\f`)
		case '\n':
			buffer.WriteString(`\n`)
		case '\r':
			buffer.WriteString(`\r`)
		case '\t':
			buffer.WriteString(`\t`)
		case '\v':
			buffer.WriteString(`\v`)
		case '"':
			buffer.WriteString(`\"`)
		case '\\':
			buffer.WriteString(`\\`)
		default:
			if r < 0x20 || r > 0x7E {
				fmt.Fprintf(buffer, `\x%02X`, r)
				continue
			}
			buffer.WriteByte(r)
		}
	}
	return `"` + buffer.String() + `"`
}

func (c *Accum) newIdent(name string, t types.Type) *ast.Ident {
	ident := ast.NewIdent(name)
	c.info.Types[ident] = types.TypeAndValue{Type: t}
	obj := types.NewVar(0, c.pkg, name, t)
	c.info.Objects[ident] = obj
	c.objectVars[obj] = name
	return ident
}

func isBlank(expr ast.Expr) bool {
	if expr == nil {
		return true
	}
	if id, isIdent := expr.(*ast.Ident); isIdent {
		return id.Name == "_"
	}
	return false
}

func (c *Accum) externalize(s string, t types.Type) string {
	if isJsObject(t) {
		return s
	}
	switch u := t.Underlying().(type) {
	case *types.Basic:
		if u.Info()&types.IsNumeric != 0 && !is64Bit(u) && u.Info()&types.IsComplex == 0 {
			return s
		}
		if u.Kind() == types.UntypedNil {
			return "null"
		}
	}
	return fmt.Sprintf("go$externalize(%s, %s)", s, c.typeName(t))
}

func (c *Accum) internalize(s string, t types.Type) string {
	if isJsObject(t) {
		return s
	}
	switch u := t.Underlying().(type) {
	case *types.Basic:
		switch {
		case u.Info()&types.IsBoolean != 0:
			return fmt.Sprintf("!!(%s)", s)
		case u.Info()&types.IsInteger != 0 && !is64Bit(u):
			return fixNumber(fmt.Sprintf("go$parseInt(%s)", s), u)
		case u.Info()&types.IsFloat != 0:
			return fmt.Sprintf("go$parseFloat(%s)", s)
		}
	}
	return fmt.Sprintf("go$internalize(%s, %s)", s, c.typeName(t))
}

func fieldName(t *types.Struct, i int) string {
	name := t.Field(i).Name()
	if name == "_" || ReservedKeywords[name] {
		return fmt.Sprintf("%s$%d", name, i)
	}
	return name
}

func (c *Accum) makeKey(expr ast.Expr, keyType types.Type) string {
	switch t := keyType.Underlying().(type) {
	case *types.Array, *types.Struct:
		return fmt.Sprintf("(new %s(%s)).go$key()", c.typeName(keyType), c.translateExpr(expr))
	case *types.Basic:
		if is64Bit(t) {
			return fmt.Sprintf("%s.go$key()", c.translateImplicitConversion(expr, keyType))
		}
		return c.translateImplicitConversion(expr, keyType)
	case *types.Chan, *types.Pointer:
		return fmt.Sprintf("%s.go$key()", c.translateImplicitConversion(expr, keyType))
	case *types.Interface:
		return fmt.Sprintf("(%s || go$interfaceNil).go$key()", c.translateImplicitConversion(expr, keyType))
	default:
		return c.translateImplicitConversion(expr, keyType)
	}
}

func (c *Accum) zeroValue(ty types.Type) string {
	switch t := ty.Underlying().(type) {
	case *types.Basic:
		switch {
		case is64Bit(t) || t.Info()&types.IsComplex != 0:
			return fmt.Sprintf("new %s(0, 0)", c.typeName(ty))
		case t.Info()&types.IsBoolean != 0:
			return "false"
		case t.Info()&types.IsNumeric != 0, t.Kind() == types.UnsafePointer:
			return "0"
		case t.Info()&types.IsString != 0:
			return `""`
		case t.Kind() == types.UntypedNil:
			panic("Zero value for untyped nil.")
		default:
			panic("Unhandled type")
		}
	case *types.Array:
		return fmt.Sprintf(`go$makeNativeArray("%s", %d, function() { return %s; })`, typeKind(t.Elem()), t.Len(), c.zeroValue(t.Elem()))
	case *types.Signature:
		return "go$throwNilPointerError"
	case *types.Slice:
		return fmt.Sprintf("%s.nil", c.typeName(ty))
	case *types.Struct:
		if named, isNamed := ty.(*types.Named); isNamed {
			return fmt.Sprintf("new %s.Ptr()", c.objectName(named.Obj()))
		}
		fields := make([]string, t.NumFields())
		for i := range fields {
			fields[i] = c.zeroValue(t.Field(i).Type())
		}
		return fmt.Sprintf("new %s.Ptr(%s)", c.typeName(ty), strings.Join(fields, ", "))
	case *types.Map:
		return "false"
	case *types.Interface:
		return "null"
	}
	return fmt.Sprintf("%s.nil", c.typeName(ty))
}

func (c *Accum) newScope(f func()) {
	outerVarNames := make(map[string]int, len(c.allVarNames))
	for k, v := range c.allVarNames {
		outerVarNames[k] = v
	}
	outerFuncVarNames := c.funcVarNames
	f()
	c.allVarNames = outerVarNames
	c.funcVarNames = outerFuncVarNames
}

func (c *Accum) translateParams(t *ast.FuncType) []string {
	params := make([]string, 0)
	for _, param := range t.Params.List {
		for _, ident := range param.Names {
			if isBlank(ident) {
				params = append(params, c.newVariable("param"))
				continue
			}
			params = append(params, c.objectName(c.info.Objects[ident]))
		}
	}
	return params
}

func (c *Accum) translateArgs(sig *types.Signature, args []ast.Expr, ellipsis bool) string {
	params := make([]string, sig.Params().Len())
	for i := range params {
		if sig.Variadic() && i == len(params)-1 && !ellipsis {
			varargType := sig.Params().At(i).Type().(*types.Slice)
			varargs := make([]string, len(args)-i)
			for j, arg := range args[i:] {
				varargs[j] = c.translateImplicitConversion(arg, varargType.Elem())
			}
			params[i] = fmt.Sprintf("new %s([%s])", c.typeName(varargType), strings.Join(varargs, ", "))
			break
		}
		argType := sig.Params().At(i).Type()
		params[i] = c.translateImplicitConversion(args[i], argType)
	}
	return strings.Join(params, ", ")
}

func (c *Accum) translateSelection(sel *types.Selection) (fields []string, jsTag string) {
	t := sel.Recv()
	for _, index := range sel.Index() {
		if ptr, isPtr := t.(*types.Pointer); isPtr {
			t = ptr.Elem()
		}
		s := t.Underlying().(*types.Struct)
		if jsTag = getJsTag(s.Tag(index)); jsTag != "" {
			for i := 0; i < s.NumFields(); i++ {
				if isJsObject(s.Field(i).Type()) {
					fields = append(fields, fieldName(s, i))
					return
				}
			}
		}
		fields = append(fields, fieldName(s, index))
		t = s.Field(index).Type()
	}
	return
}

func isWrapped(ty types.Type) bool {
	switch t := ty.Underlying().(type) {
	case *types.Basic:
		return !is64Bit(t) && t.Info()&types.IsComplex == 0 && t.Kind() != types.UntypedNil
	case *types.Array, *types.Map, *types.Signature:
		return true
	case *types.Pointer:
		_, isArray := t.Elem().Underlying().(*types.Array)
		return isArray
	}
	return false
}

func getJsTag(tag string) string {
	for tag != "" {
		// skip leading space
		i := 0
		for i < len(tag) && tag[i] == ' ' {
			i++
		}
		tag = tag[i:]
		if tag == "" {
			break
		}

		// scan to colon.
		// a space or a quote is a syntax error
		i = 0
		for i < len(tag) && tag[i] != ' ' && tag[i] != ':' && tag[i] != '"' {
			i++
		}
		if i+1 >= len(tag) || tag[i] != ':' || tag[i+1] != '"' {
			break
		}
		name := string(tag[:i])
		tag = tag[i+1:]

		// scan quoted string to find value
		i = 1
		for i < len(tag) && tag[i] != '"' {
			if tag[i] == '\\' {
				i++
			}
			i++
		}
		if i >= len(tag) {
			break
		}
		qvalue := string(tag[:i+1])
		tag = tag[i+1:]

		if name == "js" {
			value, _ := strconv.Unquote(qvalue)
			return value
		}
	}
	return ""
}

func isJsObject(t types.Type) bool {
	named, isNamed := t.(*types.Named)
	return isNamed && named.Obj().Pkg().Path() == "github.com/glycerine/bigbird/scm" && named.Obj().Name() == "Object"
}
