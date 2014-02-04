package main

// Accum and type-checking code

import (
	"fmt"
	"go/ast"
	"go/build"
	"go/token"
	"os"
	"path/filepath"
	"strings"
	"time"
	"github.com/glycerine/bigbird/translator"

	"code.google.com/p/go.tools/go/types"
)

type Accum struct {
	// from translator.PkgContext, but we can't directly
	// reuse them because they are lower-case private. Ugh.
	pkg           *types.Package
	info          *types.Info
	pkgVars       map[string]string
	objectVars    map[types.Object]string
	allVarNames   map[string]int
	funcVarNames  []string
	functionSig   *types.Signature
	resultNames   []ast.Expr
	postLoopStmt  map[string]ast.Stmt
	escapingVars  []string
	output        []byte
	delayedOutput []byte
	indentation   int
	positions     map[int]token.Pos

	pkgLine    string
	importLine []string
	preTypes   []string
	pre        []string
	goLine     []string
	post       []string

	go2sch  map[int][]int // map goLine indices to schLine indices
	schLine []string

	err error
}

func NewAccum() *Accum {
	ac := &Accum{}
	ac.pkgLine = "package main"
	ac.importLine = []string{`"fmt"`}
	ac.preTypes = []string{}
	ac.pre = []string{`func HelloBirdBrain() { fmt.Printf("HelloBirdBrain() called.\n") }`, "func main() {"}
	ac.post = []string{"}"}
	ac.ClearCode()

	return ac
}

// emptyAccum used for one-line evals
var emptyAccum *Accum = NewAccum()

func (ac *Accum) GenCode() string {
	code := ac.pkgLine + "\nimport (\n" + strings.Join(ac.importLine, "\n") +
		"\n)\n" +
		strings.Join(ac.preTypes, "\n") + "\n" +
		strings.Join(ac.pre, "\n") + "\n" +
		strings.Join(ac.goLine, "\n") + "\n" +
		strings.Join(ac.post, "\n")
	return code
}

func (ac *Accum) ClearCode() {
	ac.goLine = make([]string, 0)
	ac.go2sch = make(map[int][]int)

	ac.info = &types.Info{
		Types:      make(map[ast.Expr]types.TypeAndValue),
		Objects:    make(map[*ast.Ident]types.Object),
		Implicits:  make(map[ast.Node]types.Object),
		Selections: make(map[*ast.SelectorExpr]*types.Selection),
	}
}

type ErrorList []error

var sizes32 = &types.StdSizes{WordSize: 4, MaxAlign: 8}

// tool.go
var fileSet = token.NewFileSet()
var packages = make(map[string]*Package)

var verboseInstall = false
var packagesToTest = make(map[string]bool)
var typesPackages = make(map[string]*types.Package)

type Package struct {
	*build.Package
	SrcModTime time.Time
	UpToDate   bool
	Output     *translator.Output
}

var currentDirectory, goRoot, goPath string

func init() {
	var err error
	currentDirectory, err = os.Getwd()
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	currentDirectory, err = filepath.EvalSymlinks(currentDirectory)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
