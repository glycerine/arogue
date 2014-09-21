package main

// Accum and type-checking code

// type checking, derived from the examples in gopherjs, see gopherjs/tool.go and gopherjs/package.go
// in particular.

import (
	"fmt"
	"go/ast"
	"go/build"
	"go/parser"
	"go/scanner"
	"go/token"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/glycerine/arogue/translator"

	"code.google.com/p/go.tools/go/types"
)

func (ac *Accum) Check(fileSet *token.FileSet, files []*ast.File) ErrorList {

	var errList ErrorList
	var previousErr error

	importPkg := importPackage

	config := &types.Config{
		Packages: typesPackages,
		Import: func(_ map[string]*types.Package, path string) (*types.Package, error) {
			output, err := importPkg(path)
			if err != nil {
				return nil, err
			}
			return output.Types, nil
		},
		Sizes: sizes32,
		Error: func(err error) {
			if previousErr != nil && previousErr.Error() == err.Error() {
				return
			}
			errList = append(errList, err)
			previousErr = err
		},
	}
	typesPkg, err := config.Check("bb.go", fileSet, files, ac.info)
	ac.pkg = typesPkg

	if errList != nil {
		return errList
	}
	if err != nil {
		return []error{err}
	}

	return errList
}

func importPackage(path string) (*translator.Output, error) {
	if pkg, found := packages[path]; found {
		return pkg.Output, nil
	}

	otherPkg, err := buildImport(path, build.AllowBinary)
	if err != nil {
		return nil, err
	}
	pkg := &Package{Package: otherPkg}
	if err := buildPackage(pkg); err != nil {
		return nil, err
	}

	return pkg.Output, nil
}

func buildPackage(pkg *Package) error {

	if pkg.ImportPath == "unsafe" {
		pkg.Output = &translator.Output{Types: types.Unsafe}
		packages[pkg.ImportPath] = pkg
		return nil
	}

	if pkg.PkgObj != "" && !packagesToTest[pkg.ImportPath] {
		fileInfo, err := os.Stat(os.Args[0]) // gopherjs itself
		if err != nil {
			for _, path := range strings.Split(os.Getenv("PATH"), string(os.PathListSeparator)) {
				fileInfo, err = os.Stat(filepath.Join(path, os.Args[0]))
				if err == nil {
					break
				}
			}
			if err != nil {
				os.Stderr.WriteString("Could not get GopherJS binary's modification timestamp. Please report issue.\n")
			}
		}
		if err == nil {
			pkg.SrcModTime = fileInfo.ModTime()
		}

		for _, importedPkgPath := range pkg.Imports {
			_, err := importPackage(importedPkgPath)
			if err != nil {
				return err
			}
			impModeTime := packages[importedPkgPath].SrcModTime
			if impModeTime.After(pkg.SrcModTime) {
				pkg.SrcModTime = impModeTime
			}
		}

		for _, name := range pkg.GoFiles {
			fileInfo, err := os.Stat(filepath.Join(pkg.Dir, name))
			if err != nil {
				return err
			}
			if fileInfo.ModTime().After(pkg.SrcModTime) {
				pkg.SrcModTime = fileInfo.ModTime()
			}
		}

		pkgObjFileInfo, err := os.Stat(pkg.PkgObj)
		if err == nil && !pkg.SrcModTime.After(pkgObjFileInfo.ModTime()) {
			// package object is up to date, load from disk if library
			pkg.UpToDate = true
			if pkg.IsCommand() {
				return nil
			}

			objFile, err := ioutil.ReadFile(pkg.PkgObj)
			if err != nil {
				return err
			}

			pkg.Output, err = translator.ReadArchive(pkg.PkgObj, pkg.ImportPath, objFile)
			if err != nil {
				return err
			}
			packages[pkg.ImportPath] = pkg

			return nil
		}
	}

	if verboseInstall {
		fmt.Println(pkg.ImportPath)
	}

	var files []*ast.File
	var errList translator.ErrorList
	names := pkg.GoFiles
	if packagesToTest[pkg.ImportPath] {
		names = append(names, pkg.TestGoFiles...)
	}
	for _, name := range names {
		if pkg.ImportPath == "runtime" && strings.HasPrefix(name, "zgoarch_") {
			file, _ := parser.ParseFile(fileSet, name, "package runtime\nconst theGoarch = `js`\n", 0)
			files = append(files, file)
			continue
		}
		if pkg.ImportPath == "crypto/rc4" && name == "rc4_ref.go" { // apply patch https://codereview.appspot.com/40540049/
			file, _ := parser.ParseFile(fileSet, name, "package rc4\nfunc (c *Cipher) XORKeyStream(dst, src []byte) {\ni, j := c.i, c.j\nfor k, v := range src {\ni += 1\nj += uint8(c.s[i])\nc.s[i], c.s[j] = c.s[j], c.s[i]\ndst[k] = v ^ uint8(c.s[uint8(c.s[i]+c.s[j])])\n}\nc.i, c.j = i, j\n}\n", 0)
			files = append(files, file)
			continue
		}
		if !filepath.IsAbs(name) {
			name = filepath.Join(pkg.Dir, name)
		}
		r, err := os.Open(name)
		if err != nil {
			return err
		}
		if relname, err := filepath.Rel(currentDirectory, name); err == nil {
			name = relname
			if name[0] != '.' {
				name = "." + string(filepath.Separator) + name
			}
		}
		file, err := parser.ParseFile(fileSet, name, r, 0)
		r.Close()
		if err != nil {
			if list, isList := err.(scanner.ErrorList); isList {
				for _, entry := range list {
					errList = append(errList, entry)
				}
				continue
			}
			errList = append(errList, err)
			continue
		}
		files = append(files, file)
	}
	if errList != nil {
		return errList
	}

	var err error
	pkg.Output, err = translator.TranslatePackage(pkg.ImportPath, files, fileSet, importPackage)
	if err != nil {
		return err
	}
	packages[pkg.ImportPath] = pkg

	if pkg.ImportPath == "runtime" {
		//fmt.Println(`note: run "gopherjs install -all -v" once to speed up builds`)
	}

	return nil
}

//
// also from tool.go
//
type ImportCError struct{}

func (e *ImportCError) Error() string {
	return `importing "C" is not supported by BirdBrain`
}

func buildImport(path string, mode build.ImportMode) (*build.Package, error) {
	if path == "C" {
		return nil, &ImportCError{}
	}

	buildContext := &build.Context{
		GOROOT:   build.Default.GOROOT,
		GOPATH:   build.Default.GOPATH,
		GOOS:     build.Default.GOOS,
		GOARCH:   "js",
		Compiler: "gc",
	}
	if path == "runtime" || path == "syscall" {
		buildContext.GOARCH = build.Default.GOARCH
		buildContext.InstallSuffix = "js"
	}
	pkg, err := buildContext.Import(path, "", mode)
	if path == "hash/crc32" {
		pkg.GoFiles = []string{"crc32.go", "crc32_generic.go"}
	}
	if pkg.IsCommand() {
		pkg.PkgObj = filepath.Join(pkg.BinDir, filepath.Base(pkg.ImportPath)+".js")
	}
	if _, err := os.Stat(pkg.PkgObj); os.IsNotExist(err) && strings.HasPrefix(pkg.PkgObj, build.Default.GOROOT) {
		// fall back to GOPATH
		gopathPkgObj := build.Default.GOPATH + pkg.PkgObj[len(build.Default.GOROOT):]
		if _, err := os.Stat(gopathPkgObj); err == nil {
			pkg.PkgObj = gopathPkgObj
		}
	}
	return pkg, err
}
