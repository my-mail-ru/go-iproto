package iprotogen

import (
	"bytes"
	"errors"
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	goimports "golang.org/x/tools/imports"
)

const TmpDirPrefix = "iprotogen-"

// GetModDir returns the outermost module directory containing specified package directory
func GetModDir(pkgDir string) (string, error) {
	dir := pkgDir
	modDir := ""

	for {
		goMod := filepath.Join(dir, "go.mod")
		_, err := os.Stat(goMod)

		if err == nil {
			modDir = dir
		} else if !errors.Is(err, os.ErrNotExist) {
			return "", fmt.Errorf("%s: %w", goMod, err)
		}

		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}

		dir = parent
	}

	if modDir == "" {
		return "", errors.New(pkgDir + " isn't contained by a go module")
	}

	return modDir, nil
}

// StripModule copies the module to a temporary location preserving only:
//   - go.mod and go.sum files
//   - all imports
//   - all type and const declarations
//   - all methods (replacing func bodies with a bare return or an empty block depending on values returned)
//   - all funcs named main (regardless of their params, bodies are stripped too)
//   - all comments on these entities
//
// Anything other (e.g. vars, non-method funcs) is removed.
//
// If a directory was created successfully it is returned always, even in the case of an error.
func StripModule(modPath string, needTests bool) (string, error) {
	modPath, err := filepath.Abs(modPath)
	if err != nil {
		return "", fmt.Errorf("Abs(%s): %w", modPath, err)
	}

	modName := filepath.Base(modPath) // not actually a module name from go.mod

	if modName == modPath {
		modName = "root" // go.mod in a root dir, a very crazy case
	}

	tmpDirOrig, err := os.MkdirTemp("", TmpDirPrefix+modName+"-")
	if err != nil {
		return "", err
	}

	tmpDir, err := filepath.EvalSymlinks(tmpDirOrig) // go parser doesn't grok symlinks
	if err != nil {
		return tmpDirOrig, err
	}

	fset := token.NewFileSet()

	err = filepath.WalkDir(modPath, func(path string, info fs.DirEntry, err error) error {
		if err != nil {
			return err
		}

		if path == modPath {
			return nil
		}

		fname := info.Name()
		if fname[0] == '.' {
			if info.IsDir() {
				return fs.SkipDir
			}

			return nil
		}

		newPath, err := RebasePath(modPath, tmpDir, path)
		if err != nil {
			return err
		}

		if info.IsDir() {
			if err := os.Mkdir(newPath, 0700); err != nil {
				return err
			}

			return nil
		}

		if fname == "go.mod" || fname == "go.sum" {
			return copyFile(newPath, path, 0600)
		}

		if strings.HasSuffix(fname, ".go") && (needTests || !strings.HasSuffix(fname, "_test.go")) {
			return stripGoFile(fset, newPath, path, 0600)
		}

		return nil
	})

	return tmpDir, err
}

func StripPackage(pkgDir string, needTests bool) (string, string, error) {
	pkgDir, err := filepath.Abs(pkgDir)
	if err != nil {
		return "", "", fmt.Errorf("Abs(%s): %w", pkgDir, err)
	}

	modDir, err := GetModDir(pkgDir)

	if err != nil {
		return "", "", err
	}

	newModDir, err := StripModule(modDir, needTests)
	if err != nil {
		return newModDir, "", err
	}

	newPkgDir, err := RebasePath(modDir, newModDir, pkgDir)
	if err != nil {
		return newModDir, "", err
	}

	return newModDir, newPkgDir, nil
}

func copyFile(dst, src string, mode fs.FileMode) error {
	s, err := os.Open(src)
	if err != nil {
		return fmt.Errorf("copyFile: %s: %w", src, err)
	}

	defer s.Close()

	d, err := os.OpenFile(dst, os.O_WRONLY|os.O_CREATE, mode)
	if err != nil {
		return fmt.Errorf("copyFile: %s: %w", dst, err)
	}

	defer d.Close()

	if _, err := io.Copy(d, s); err != nil {
		return fmt.Errorf("copyFile: %s: %w", dst, err)
	}

	return nil
}

func stripGoFile(fset *token.FileSet, dst, src string, mode fs.FileMode) error {
	fSrc, err := parser.ParseFile(fset, src, nil, parser.ParseComments|parser.SkipObjectResolution)
	if err != nil {
		return fmt.Errorf("%s: %w", src, err)
	}

	wDst, err := os.OpenFile(dst, os.O_WRONLY|os.O_CREATE, mode)
	if err != nil {
		return fmt.Errorf("%s: %w", dst, err)
	}

	defer wDst.Close()

	fDst := &ast.File{
		Name:      fSrc.Name,
		Imports:   fSrc.Imports,
		GoVersion: fSrc.GoVersion,
		Doc:       fSrc.Doc, // will print package-level comment in between package keyword and the package name. pkgDoc supports this.
		Comments:  StripComments(fSrc.Comments),
		Decls:     make([]ast.Decl, 0, len(fSrc.Decls)),
	}

	for _, decl := range fSrc.Decls {
		switch d := decl.(type) {
		case *ast.GenDecl:
			switch d.Tok {
			default:
				continue
			case token.IMPORT, token.CONST, token.TYPE:
			}

		case *ast.FuncDecl:
			if d.Recv == nil && d.Name.Name != "main" {
				continue
			}

			d.Body = &ast.BlockStmt{}

			if d.Type.Results != nil {
				if len(d.Type.Results.List[0].Names) == 0 { // correct go func return values must be either all named or all unnamed
					for i := range d.Type.Results.List {
						d.Type.Results.List[i].Names = []*ast.Ident{ast.NewIdent("dummyRetVal" + strconv.Itoa(i))}
					}
				}

				d.Body.List = []ast.Stmt{&ast.ReturnStmt{}}
			}

		default:
			continue
		}

		fDst.Decls = append(fDst.Decls, decl)
	}

	buf := &bytes.Buffer{}
	err = printer.Fprint(buf, fset, fDst)

	if err != nil {
		return fmt.Errorf("go/printer: %s: %w", dst, err)
	}

	bytes, err := goimports.Process(dst, buf.Bytes(), nil)
	if err != nil {
		return fmt.Errorf("goimports: %s: %w", dst, err)
	}

	if _, err := wDst.Write(bytes); err != nil {
		return fmt.Errorf("write %s: %w", dst, err)
	}

	return nil
}

// TODO XXX prefixes aren't reliable, even [filepath.Abs]'ed ones
// refactor using cyclic filepath.Base
func RebasePath(base, newBase, file string) (string, error) {
	if base == newBase {
		return file, nil
	}

	relPath, ok := strings.CutPrefix(file, base)
	if !ok {
		return "", errors.New("file path " + file + " doesn't contain " + base + " prefix")
	}

	return filepath.Join(newBase, relPath), nil
}

// StripComments keeps:
//   - directives
//   - iprotogen banner
//
// All other comments are removed.
func StripComments(groups []*ast.CommentGroup) []*ast.CommentGroup {
	ret := make([]*ast.CommentGroup, 0, 16) // directives are rare

	for _, group := range groups {
		list := ([]*ast.Comment)(nil) // ... so very rare that preallocation will hurt

		for _, comment := range group.List {
			if strings.HasPrefix(comment.Text, iprotogenBanner) || directiveRegexp.MatchString(comment.Text) {
				list = append(list, comment)
			}
		}

		if len(list) != 0 {
			ret = append(ret, &ast.CommentGroup{List: list})
		}
	}

	return ret
}
