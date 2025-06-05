package iprotogen

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"log"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"sync"

	"github.com/fatih/structtag"
	"golang.org/x/tools/go/packages"
)

const (
	// IProtoDirective - generate [iproto.MarshalerUnmarshaler] implementation for types marked by this special comment.
	IProtoDirective = "//adv:iproto:"

	// IProtoTag - struct tag for overriding the field's representation
	IProtoTag = "iproto"
)

const (
	iprotoPackage   = "github.com/my-mail-ru/go-iproto"
	vendorPrefixLen = len("//adv:")
)

type CommentsByIdent struct {
	m sync.Map
}

func (cbi *CommentsByIdent) FromFile(fset *token.FileSet, f *ast.File) {
	cm := ast.NewCommentMap(fset, f, f.Comments)
	if cm == nil {
		return
	}

	for node, cgs := range cm {
		switch node := node.(type) {
		case *ast.GenDecl:
			if node.Tok != token.TYPE {
				continue
			}

			typeDoc := getComment(cgs) // doc before the type keyword

			for _, spec := range node.Specs { // may overwrite TypeSpecs seen before (because of the nature of map). it's ok.
				spec, ok := spec.(*ast.TypeSpec)
				if !ok {
					continue
				}

				if nested, ok := cm[spec]; ok { // have a doc before (or a line comment after) the exact type spec
					cbi.set(spec.Name, getComment(nested))
					delete(cm, spec)
				} else { // toplevel doc only
					cbi.set(spec.Name, typeDoc)
				}
			}

		case *ast.TypeSpec:
			cbi.set(node.Name, getComment(cgs))
		}
	}
}

func (cbi *CommentsByIdent) set(name *ast.Ident, comment string) {
	if comment == "" {
		return // no comments, not the empty //adv:iproto: comment
	}

	cbi.m.Store(name, strings.TrimSpace(comment))
}

func (cbi *CommentsByIdent) Get(name *ast.Ident) (*structtag.Tag, error) {
	sI, ok := cbi.m.Load(name)
	if !ok {
		return nil, nil
	}

	s := sI.(string)

	if s == IProtoDirective[vendorPrefixLen:] {
		return &structtag.Tag{}, nil
	}

	tags, err := structtag.Parse(s)
	if err != nil {
		return nil, err
	}

	tag, err := tags.Get(IProtoTag)
	if err != nil {
		return nil, err // "tag not found" should be treated as an error here
	}

	return tag, nil
}

func (cbi *CommentsByIdent) FromTypesInfo(fset *token.FileSet, ti *types.Info) {
	for node := range ti.Scopes {
		if f, ok := node.(*ast.File); ok {
			cbi.FromFile(fset, f)
		}
	}
}

func getComment(cgs []*ast.CommentGroup) string {
	for _, cg := range cgs {
		for _, comment := range cg.List {
			if strings.HasPrefix(comment.Text, IProtoDirective) {
				return comment.Text[vendorPrefixLen:]
			}
		}
	}

	return ""
}

type FilesByPath map[string]*File

func (fbp FilesByPath) Get(filePath, pkgName string, directivesByFile map[string]string) *File {
	if f, ok := fbp[filePath]; ok {
		return f
	}

	byPkg, byAlias := newImportMaps()

	f := &File{
		PkgName:     pkgName,
		ImportByPkg: byPkg,
		ImportAlias: byAlias,
		Directives:  directivesByFile[filePath],
		Decls:       make([]Decl, 0, 10),
	}

	fbp[filePath] = f

	return f
}

func (f *File) AddType(ident *ast.Ident, typ Type) {
	f.Decls = append(f.Decls, Decl{
		TypeIdent: ident,
		Type:      typ,
	})
}

// TypeToExpr is a design error and should be eliminated.
func (f *File) TypeToExpr(typ types.Type, pos token.Pos) (ast.Expr, error) {
	typeStr := types.TypeString(typ, func(pkg *types.Package) string {
		pkgName := pkg.Name()
		if pkgName == f.PkgName {
			return ""
		}

		pkgPath := pkg.Path()
		if alias, ok := f.ImportByPkg[pkgPath]; ok {
			if alias == "" {
				return pkgName
			}

			return alias
		}

		i := 0
		suffix := ""

		for {
			alias := pkgName + suffix
			if _, ok := f.ImportAlias[alias]; !ok {
				f.ImportByPkg[pkgPath] = alias
				f.ImportAlias[alias] = struct{}{}

				return alias
			}

			i++
			suffix = strconv.Itoa(i)
		}
	})

	// return ast.NewIdent(typeStr), nil // produces incorrect AST (generated code is ok)
	expr, err := parser.ParseExpr(typeStr) // ... but this is slower
	if err != nil {
		return nil, err
	}

	setExprPos(expr, pos)

	return expr, nil
}

// we can't wrap the Pos() using a custom type because the go/printer is full of type switches.
func setExprPos(expr ast.Expr, pos token.Pos) {
	// only the nodes that can be used inside a type expression
	switch x := expr.(type) {
	case *ast.Ident: // simple types
		x.NamePos = pos
	case *ast.StarExpr: // pointer types
		x.Star = pos
	case *ast.SelectorExpr: // package.type
		setExprPos(x.X, pos)
	case *ast.CompositeLit: // not sure
		if x.Type != nil {
			setExprPos(x.Type, pos)
		} else {
			x.Lbrace = pos
		}
	case *ast.ArrayType:
		x.Lbrack = pos
	case *ast.StructType:
		x.Struct = pos
	case *ast.MapType:
		x.Map = pos
	}
}

type Parser struct {
	fset            *token.FileSet
	onlyFile        sameFileChecker
	generated       sync.Map
	modDir          string
	buildFlags      []string
	ignoreGenerated bool
	parseTests      bool
}

type pkgParser struct {
	*Parser
	file           *File
	pkgs           []*packages.Package
	ifaceMarshal   *types.Interface
	ifaceUnmarshal *types.Interface
}

type pkgParserOptions struct {
	onlyFile        string
	modDir          string
	buildFlags      []string
	ignoreGenerated bool
	parseTests      bool
}

type ParserOptionsFunc func(*pkgParserOptions)

func WithOnlyFile(fname string) ParserOptionsFunc {
	return func(opt *pkgParserOptions) {
		opt.onlyFile = fname
	}
}

func WithModuleDir(dir string) ParserOptionsFunc {
	return func(opt *pkgParserOptions) {
		opt.modDir = dir
	}
}

func WithIgnoreGenerated(opt *pkgParserOptions) {
	opt.ignoreGenerated = true
}

func WithParseTests(opt *pkgParserOptions) {
	opt.parseTests = true
}

func WithBuildFlag(flag string) ParserOptionsFunc {
	return func(opt *pkgParserOptions) {
		opt.buildFlags = append(opt.buildFlags, flag)
	}
}

func NewParser(optFuncs ...ParserOptionsFunc) (*Parser, error) {
	var opt pkgParserOptions

	for _, f := range optFuncs {
		f(&opt)
	}

	ret := &Parser{
		fset:            token.NewFileSet(),
		modDir:          opt.modDir,
		buildFlags:      opt.buildFlags,
		ignoreGenerated: opt.ignoreGenerated,
		parseTests:      opt.parseTests,
	}

	if opt.onlyFile != "" {
		onlyFile, err := newSameFileChecker(opt.onlyFile)
		if err != nil {
			return nil, err
		}

		ret.onlyFile = onlyFile
	}

	return ret, nil
}

func (p *Parser) newParser(pkgs []*packages.Package) (*pkgParser, error) {
	ret := &pkgParser{
		Parser: p,
		pkgs:   make([]*packages.Package, 0, len(pkgs)),
	}

	for _, pkg := range pkgs {
		if pkg.PkgPath == iprotoPackage {
			var err error

			ret.ifaceMarshal, err = getIface(pkg, "Marshaler")
			if err != nil {
				return nil, err
			}

			ret.ifaceUnmarshal, err = getIface(pkg, "Unmarshaler")
			if err != nil {
				return nil, err
			}
		} else {
			ret.pkgs = append(ret.pkgs, pkg)
		}
	}

	if ret.ifaceMarshal != nil && ret.ifaceUnmarshal != nil {
		return ret, nil
	}

	return nil, errors.New("package " + iprotoPackage + " not loaded")
}

// DefType describes a defined type
type DefType struct {
	ident *ast.Ident
	typ   *types.TypeName
}

func (p *Parser) ParsePackage(pkgName string) (FilesByPath, error) {
	pkgs, directivesByFile, err := p.loadPackages(p.fset, pkgName, iprotoPackage)
	if err != nil {
		return nil, err
	}

	pp, err := p.newParser(pkgs)
	if err != nil {
		return nil, err
	}

	ret := make(FilesByPath)
	seen := make(map[string]struct{}) // we can omit the object position since we deal with types only (so the name is unique)

	for _, pkg := range pp.pkgs {
		if err = pp.parseTypes(pkg, ret, seen, directivesByFile); err != nil {
			return nil, err
		}
	}

	return ret, nil
}

// poser is a half of the ast.Node.
// this type is required because types.Var lacks the End() method
type poser interface {
	Pos() token.Pos
}

type errorWithPos struct {
	pos poser
	err error
}

func newErrorWithPos(pos poser, err error) *errorWithPos {
	var ewp *errorWithPos

	if errors.As(err, &ewp) { // preserve the line information from the deepest level
		pos = ewp.pos
	}

	return &errorWithPos{
		pos: pos,
		err: err,
	}
}

func (ewp *errorWithPos) Unwrap() error {
	return ewp.err
}

func (ewp *errorWithPos) Error() string {
	return ewp.err.Error()
}

func (pp *pkgParser) parseTypes(pkg *packages.Package, filesByPath FilesByPath, seen map[string]struct{}, directivesByFile map[string]string) error {
	comments := CommentsByIdent{}
	comments.FromTypesInfo(pp.Parser.fset, pkg.TypesInfo)

	deftypes := make([]DefType, 0, len(pkg.TypesInfo.Defs))

	for ident, typ := range pkg.TypesInfo.Defs {
		deftype, ok := typ.(*types.TypeName) // drop non-types. now pkg + name is unique
		if !ok {
			continue
		}

		k := pkg.PkgPath + "$" + ident.Name
		if _, ok := seen[k]; ok {
			continue
		}

		deftypes = append(deftypes, DefType{
			ident: ident,
			typ:   deftype,
		})

		seen[k] = struct{}{}
	}

	// make generated code reproducible
	slices.SortFunc(deftypes, func(a, b DefType) int {
		return strings.Compare(a.ident.Name, b.ident.Name)
	})

	for _, deftype := range deftypes {
		filePath := pp.Parser.fset.Position(deftype.ident.Pos()).Filename
		if pp.Parser.onlyFile.fname != "" {
			ok, err := pp.Parser.onlyFile.check(filePath)
			if err != nil {
				return err
			}

			if !ok {
				continue
			}
		}

		pp.file = filesByPath.Get(filePath, pkg.Name, directivesByFile)

		tag, err := comments.Get(deftype.ident)
		if err != nil {
			return err
		}

		if deftype.typ.IsAlias() {
			if tag != nil {
				return errors.New(pp.filenameLine(deftype.ident) + ": " + deftype.ident.Name + ": cannot generate methods for type alias")
			}

			continue
		}

		if tag == nil || isSkipped(tag.Name) {
			continue
		}

		typ, err := pp.parseType(deftype.ident, deftype.typ.Type(), tag, false)
		if err != nil {
			pos := poser(deftype.ident)
			ewp := (*errorWithPos)(nil)

			if errors.As(err, &ewp) {
				pos = ewp.pos
			}

			return fmt.Errorf("%s: %s.%w", pp.filenameLine(pos), pkg.Name, err)
		}

		pp.file.AddType(deftype.ident, typ)
	}

	return nil
}

func (p *Parser) loadPackages(fset *token.FileSet, pkgPattern ...string) ([]*packages.Package, map[string]string, error) {
	// used to copy orig file's directives (esp. build tags) to the generated file
	directivesByFile := make(map[string]string)
	directivesByFileMu := sync.Mutex{}

	cfg := &packages.Config{
		// TODO remove unused
		Mode:       packages.NeedName | packages.NeedImports | packages.NeedTypes /*| packages.NeedSyntax*/ | packages.NeedTypesInfo,
		Fset:       fset,
		Tests:      p.parseTests,
		BuildFlags: p.buildFlags,
		ParseFile: func(fset *token.FileSet, filename string, src []byte) (*ast.File, error) {
			mode := parser.ParseComments | parser.SkipObjectResolution
			directives, isGenerated := pkgDoc(src)

			if isGenerated {
				p.generated.Store(filename, nil)

				if p.ignoreGenerated {
					mode = parser.PackageClauseOnly | parser.SkipObjectResolution
				}
			}

			file, err := parser.ParseFile(fset, filename, src, mode)
			if err != nil {
				return nil, fmt.Errorf("parser.ParseFile(%s): %w", filename, err)
			}

			// TODO delete handwritten func bodies too
			// TODO this approach doesn't work as expected, disabled in favor of [StripPackage]
			/* if isGenerated {
				// using recursion because shallow iteration (file.Decls) may omit toplevel anon funcs (var f = func(){...})
				file.Imports = nil
				ast.Walk(ignoreFuncBodies{}, file)
			} */

			if directives != "" {
				directivesByFileMu.Lock()
				directivesByFile[filename] = directives
				directivesByFileMu.Unlock()
			}

			return file, nil
		},
	}

	pkgs, err := packages.Load(cfg, pkgPattern...)
	if err != nil {
		return nil, nil, fmt.Errorf("packages.Load: %w", err)
	}

	var errs []error

	for _, pkg := range pkgs {
		if len(pkg.Errors) != 0 {
			for _, err := range pkg.Errors {
				errs = append(errs, fmt.Errorf("package %s: %w", pkg.Name, err))
			}
		}
	}

	if len(errs) != 0 {
		return nil, nil, errors.Join(errs...)
	}

	return pkgs, directivesByFile, nil
}

/*
type ignoreFuncBodies struct{}

func (ignoreFuncBodies) Visit(node ast.Node) ast.Visitor {
	if node == nil {
		return nil
	}

	if decl, ok := node.(*ast.GenDecl); ok && decl.Tok == token.IMPORT {
		decl.Specs = nil
	}

	funcDecl, ok := node.(*ast.FuncDecl)
	if !ok {
		return ignoreFuncBodies{}
	}

	funcDecl.Body = nil

	return nil
}
*/

// relaxed version of ast.isDirective
var directiveRegexp = regexp.MustCompile(`^//\S`)

func pkgDoc(src []byte) (directives string, isGeneratedByMe bool) {
	sc := bufio.NewScanner(bytes.NewReader(src))
	sb := strings.Builder{}

LINES:
	for sc.Scan() {
		switch line := strings.TrimSpace(sc.Text()); {
		case strings.HasPrefix(line, iprotogenBanner):
			return "", true
		case directiveRegexp.MatchString(line): // directive
			sb.WriteString(line)
			sb.WriteRune('\n')
		case !strings.HasPrefix(line, "//") && line != "" && line != "package": // not a doc comment or a blank line. a line containing only the package keyword is allowed because go/printer prints package-level comments in a bizarre way.
			break LINES
		}
	}

	return sb.String(), false
}

func getIface(pkg *packages.Package, name string) (*types.Interface, error) {
	obj := pkg.Types.Scope().Lookup(name)
	if obj == nil {
		return nil, errors.New("object " + name + " not found in package " + pkg.Name)
	}

	iface, ok := obj.Type().Underlying().(*types.Interface)
	if !ok {
		return nil, errors.New("type " + pkg.Name + "." + name + " is not an interface")
	}

	return iface, nil
}

func isSkipped(tagName string) bool {
	return tagName == "-" || tagName == "skip"
}

type sameFileChecker struct {
	fname string
	finfo os.FileInfo
}

// always use abs file names
func newSameFileChecker(fname string) (sameFileChecker, error) {
	finfo, err := os.Stat(fname)
	if err != nil {
		return sameFileChecker{}, err
	}

	return sameFileChecker{
		fname: fname,
		finfo: finfo,
	}, nil
}

func (sf sameFileChecker) check(fname string) (bool, error) {
	if sf.fname == fname {
		return true, nil
	}

	finfo, err := os.Stat(fname)
	if err != nil {
		return false, err
	}

	return os.SameFile(sf.finfo, finfo), nil
}

func (pp *pkgParser) parseType(expr ast.Expr, goType types.Type, tag *structtag.Tag, needStructLit bool) (Type, error) {
	marshalerRecvType := goType
	unmarshalerRecvType := goType
	goType = goType.Underlying()

	if _, isIface := goType.(*types.Interface); !isIface {
		unmarshalerRecvType = types.NewPointer(unmarshalerRecvType) // require pointer receiver for non-interface types
	}

	hasMarshaler := pp.hasHandwrittenImpl(marshalerRecvType, pp.ifaceMarshal)
	hasUnmarshaler := pp.hasHandwrittenImpl(unmarshalerRecvType, pp.ifaceUnmarshal)

	if hasMarshaler || hasUnmarshaler {
		return Custom{
			hasMarshaler:   hasMarshaler,
			hasUnmarshaler: hasUnmarshaler,
		}, nil
	}

	var (
		ret Type
		err error
	)

	switch goType := goType.(type) {
	case *types.Basic:
		ret, err = pp.parseBasic(expr, goType, tag)
	case *types.Slice:
		ret, err = pp.parseSlice(expr, goType, tag)
	case *types.Array:
		ret, err = pp.parseArray(expr, goType, tag)
	case *types.Map:
		ret, err = pp.parseMap(expr, goType, tag)
	case *types.Struct:
		ret, err = pp.parseStruct(expr, goType, needStructLit)
	case *types.Pointer:
		ret, err = pp.parsePointer(expr, goType, tag)

	default:
		return nil, fmt.Errorf("%s: unsupported type %v (%T)", astToSource(expr), goType, goType)
	}

	if err != nil {
		return nil, fmt.Errorf("%s: %w", astToSource(expr), err)
	}

	return ret, nil
}

type intParams struct {
	size int
	min  int64
	max  uint64
}

// most common used type tags
const (
	u8  = "u8"
	u32 = "u32"
)

var intTags = map[string]intParams{
	u8:    {size: 1, max: 255},
	"u16": {size: 2, max: 65535},
	u32:   {size: 4, max: 0xFFFFFFFF},
	"u64": {size: 8},
	"i8":  {size: 1, min: -128, max: 127},
	"i16": {size: 2, min: -32768, max: 32767},
	"i32": {size: 4, min: -0x80000000, max: 0x7FFFFFFF},
	"i64": {size: 8},
	"ber": {size: 0},
}

var intDefaultTag = map[types.BasicKind]string{
	types.Int8:   "i8",
	types.Int16:  "i16",
	types.Int32:  "i32",
	types.Int64:  "i64",
	types.Uint8:  u8,
	types.Uint16: "u16",
	types.Uint32: u32,
	types.Uint64: "u64",
	types.Int:    "", // no default tag
	types.Uint:   "", //
}

func (pp *pkgParser) parseInt(expr, warnLenExpr ast.Expr, intTag, dflTag string) (Integer, error) {
	if intTag == "" {
		if dflTag == "" {
			return Integer{}, errors.New("tag is required for int/uint types")
		}

		if warnLenExpr != nil {
			log.Printf("[WARN] %s: using the default length format %q for %s", pp.filenameLine(warnLenExpr), dflTag, astToSource(warnLenExpr))
		}

		intTag = dflTag
	}

	params, ok := intTags[intTag]
	if !ok {
		return Integer{}, fmt.Errorf("%v: unknown tag %s", expr, intTag)
	}

	if dflTag != "" {
		dflParams, ok := intTags[dflTag]
		if !ok {
			return Integer{}, errors.New("parseInt: internal error: default tag not found: " + dflTag)
		}

		if params.size >= dflParams.size {
			params.min = 0
			params.max = 0
		}
	}

	return Integer{
		TypeExpr: expr,
		Size:     params.size,
		Min:      params.min,
		Max:      params.max,
	}, nil
}

func (pp *pkgParser) parseBasic(expr ast.Expr, basic *types.Basic, tag *structtag.Tag) (Type, error) {
	kind := basic.Kind()

	if dflTag, isInt := intDefaultTag[kind]; isInt {
		if len(tag.Options) != 0 {
			return nil, fmt.Errorf("%v: unsupported tag options for %s: %v", expr, basic.Name(), tag.Options)
		}

		return pp.parseInt(expr, nil, tag.Name, dflTag)
	}

	switch kind {
	case types.Bool:
		boolOpts := make([]string, len(tag.Options)+1)
		boolOpts[0] = tag.Name
		copy(boolOpts[1:], tag.Options)

		return parseBool(boolOpts...)
	case types.Float32:
		if tag.Name != "" {
			return nil, fmt.Errorf("%v: unsupported tag for float32: %s", expr, tag.Name)
		}

		if len(tag.Options) != 0 {
			return nil, fmt.Errorf("%v: unsupported tag options for float32: %v", expr, tag.Options)
		}

		return Float{TypeExpr: expr, Size: 4}, nil
	case types.Float64:
		if tag.Name != "" {
			return nil, fmt.Errorf("%v: unsupported tag for float64: %s", expr, tag.Name)
		}

		if len(tag.Options) != 0 {
			return nil, fmt.Errorf("%v: unsupported tag options for float64: %v", expr, tag.Options)
		}

		return Float{TypeExpr: expr, Size: 8}, nil
	case types.String:
		lenType, err := pp.parseInt(identInt, expr, tag.Name, u32)
		if err != nil {
			return nil, err
		}

		if len(tag.Options) != 0 {
			return nil, fmt.Errorf("%v: unsupported tag options for string: %v", expr, tag.Options)
		}

		return StringOrBytes{TypeExpr: expr, LenType: lenType}, nil
	default:
		return nil, fmt.Errorf("unsupported basic type kind: %d", basic.Kind())
	}
}

func parseBool(opts ...string) (Bool, error) {
	valTrue := 1
	valFalse := 0

	for _, opt := range opts {
		opt = strings.TrimSpace(opt)
		if opt == "" {
			continue
		}

		name, value, ok := strings.Cut(opt, ":")
		if !ok {
			return Bool{}, fmt.Errorf("error parsing bool spec %q: ':' not found", opt)
		}

		name = strings.ToLower(strings.TrimSpace(name))
		value = strings.TrimSpace(value)

		if value == "" {
			return Bool{}, fmt.Errorf("error parsing bool spec %q: value must not be empty", opt)
		}

		var intVal int

		if value[0] == '\'' {
			if len(value) != 3 || value[2] != '\'' {
				return Bool{}, fmt.Errorf("error parsing bool spec %q: single-byte-char notation is 'c'", opt)
			}

			intVal = int(value[1])
		} else {
			v, err := strconv.ParseUint(value, 0, 8) // 8 bits ensures bound checks for the uint8 type
			if err != nil {
				return Bool{}, fmt.Errorf("error parsing bool spec %q: %w", opt, err)
			}

			intVal = int(v)
		}

		switch name {
		case "true":
			valTrue = intVal
		case "false":
			valFalse = intVal
		default:
			return Bool{}, fmt.Errorf("error parsing bool spec %q: unknown name %q: only true and false are supported", opt, name)
		}
	}

	if valTrue == valFalse {
		return Bool{}, fmt.Errorf("error parsing bool spec: true must differ from false (both values are %d)", valTrue)
	}

	return Bool{
		True:  litInt(valTrue),
		False: litInt(valFalse),
	}, nil
}

func (pp *pkgParser) parseSlice(expr ast.Expr, sl *types.Slice, tag *structtag.Tag) (Type, error) {
	lenType, err := pp.parseInt(identInt, expr, tag.Name, u32)
	if err != nil {
		return nil, err
	}

	elemTag := &structtag.Tag{Key: tag.Key}

	if len(tag.Options) != 0 {
		elemTag.Name = tag.Options[0]
		elemTag.Options = tag.Options[1:]
	}

	elemGoType := sl.Elem()

	if elemTag.Name == "" || elemTag.Name == u8 {
		// check the exact (not an underlying) type - a conversion from []byte to []Defined will not work
		if basic, ok := elemGoType.(*types.Basic); ok && basic.Kind() == types.Uint8 {
			return StringOrBytes{
				TypeExpr: expr,
				LenType:  lenType,
				IsSlice:  true,
			}, nil
		}
	}

	pos := expr.Pos()

	elemGoTypeExpr, err := pp.file.TypeToExpr(elemGoType, pos)
	if err != nil {
		return nil, err
	}

	elemType, err := pp.parseType(elemGoTypeExpr, elemGoType, elemTag, false)
	if err != nil {
		return nil, err
	}

	typeExpr, err := pp.file.TypeToExpr(sl, pos)
	if err != nil {
		return nil, err
	}

	return Slice{
		TypeExpr: typeExpr,
		LenType:  lenType,
		ElemType: elemType,
	}, nil
}

func (pp *pkgParser) parseArray(expr ast.Expr, arrType *types.Array, elemTag *structtag.Tag) (Type, error) {
	elemGoType := arrType.Elem()
	lenExpr := litInt64(arrType.Len())

	if elemTag.Name == "" || elemTag.Name == u8 {
		// check the exact (not an underlying) type - a conversion from [...]byte to [...]Defined will not work
		if basic, ok := elemGoType.(*types.Basic); ok && basic.Kind() == types.Uint8 {
			return ByteArray{
				Len:      lenExpr,
				TypeExpr: expr,
			}, nil
		}
	}

	elemGoTypeExpr, err := pp.file.TypeToExpr(elemGoType, expr.Pos())
	if err != nil {
		return nil, err
	}

	elemType, err := pp.parseType(elemGoTypeExpr, elemGoType, elemTag, false)
	if err != nil {
		return nil, err
	}

	return Array{
		Len:      lenExpr,
		ElemType: elemType,
	}, nil
}

// only one key tag is supported:
// iproto:lentag,keytag,valuetag1,valuetag2...
func (pp *pkgParser) parseMap(expr ast.Expr, mapType *types.Map, tag *structtag.Tag) (Type, error) {
	lenType, err := pp.parseInt(identInt, expr, tag.Name, u32)
	if err != nil {
		return nil, err
	}

	keyTag := &structtag.Tag{Key: tag.Key}
	valTag := &structtag.Tag{Key: tag.Key}

	if len(tag.Options) != 0 {
		keyTag.Name = tag.Options[0]

		if len(tag.Options) > 1 {
			valTag.Name = tag.Options[1]
			valTag.Options = tag.Options[2:]
		}
	}

	pos := expr.Pos()
	keyGoType := mapType.Key()
	valGoType := mapType.Elem()

	keyGoTypeExpr, err := pp.file.TypeToExpr(keyGoType, pos)
	if err != nil {
		return nil, err
	}

	valGoTypeExpr, err := pp.file.TypeToExpr(valGoType, pos)
	if err != nil {
		return nil, err
	}

	keyType, err := pp.parseType(keyGoTypeExpr, keyGoType, keyTag, true)
	if err != nil {
		return nil, err
	}

	valType, err := pp.parseType(valGoTypeExpr, valGoType, valTag, true)
	if err != nil {
		return nil, err
	}

	return Map{
		LenType:   lenType,
		TypeExpr:  expr,
		KeyType:   keyType,
		ValueType: valType,
	}, nil
}

func (pp *pkgParser) parseStruct(expr ast.Expr, st *types.Struct, needStructLit bool) (Type, error) {
	numFields := st.NumFields()
	fields := make([]StructField, 0, numFields)

	for i := range numFields {
		field := st.Field(i)
		fieldName := field.Name()

		tag := (*structtag.Tag)(nil)

		if allTags := st.Tag(i); allTags != "" {
			tags, err := structtag.Parse(allTags)
			if err != nil {
				return nil, newErrorWithPos(field, fmt.Errorf("field %s has invalid tag %s: %w", fieldName, allTags, err))
			}

			tag, _ = tags.Get(IProtoTag)
		}

		noTag := tag == nil
		if noTag {
			tag = &structtag.Tag{}
		}

		if isSkipped(tag.Name) {
			continue
		}

		if !field.Exported() {
			if fieldName == "_" || noTag {
				continue
			}

			return nil, newErrorWithPos(field, errors.New("non-exported field "+fieldName+" has iproto tag"))
		}

		goType := field.Type()

		typeExpr, err := pp.file.TypeToExpr(goType, field.Pos())
		if err != nil {
			return nil, newErrorWithPos(field, fmt.Errorf("field %s: %w", fieldName, err))
		}

		typ, err := pp.parseType(typeExpr, goType, tag, false)
		if err != nil {
			return nil, newErrorWithPos(field, fmt.Errorf("field %s: %w", fieldName, err))
		}

		fields = append(fields, StructField{
			Ident: ast.NewIdent(fieldName),
			Type:  typ,
		})
	}

	if needStructLit {
		return StructLiteral{
			Fields:   fields,
			TypeExpr: expr,
		}, nil
	}

	return Struct{Fields: fields}, nil
}

func (pp *pkgParser) parsePointer(expr ast.Expr, ptr *types.Pointer, tag *structtag.Tag) (Type, error) {
	elemGoType := ptr.Elem()
	elemGoTypeExpr, err := pp.file.TypeToExpr(ptr, expr.Pos())

	if err != nil {
		return nil, err
	}

	elemType, err := pp.parseType(elemGoTypeExpr, elemGoType, tag, false)
	if err != nil {
		return nil, err
	}

	return Pointer{
		Type:     elemType,
		TypeExpr: elemGoTypeExpr,
	}, nil
}

func (p *Parser) filenameLine(poser poser) string {
	pos := poser.Pos()
	if pos == token.NoPos {
		return "<UNKNOWN>" // the node's ast type isn't supported by setExprPos
	}

	position := p.fset.Position(pos)
	filename := position.Filename

	if p.modDir != "" {
		if fname, err := filepath.Rel(p.modDir, filename); err == nil {
			filename = fname
		}
	}

	return filename + ":" + strconv.Itoa(position.Line)
}

func (p *Parser) hasHandwrittenImpl(typ types.Type, iface *types.Interface) bool {
	if !types.Implements(typ, iface) {
		return false // typ doesn't implement iface at all
	}

	if ptr, ok := typ.(*types.Pointer); ok {
		typ = ptr.Elem()
	}

	named, ok := typ.(*types.Named)
	if !ok {
		return false // typ isn't a defined type
	}

	// TODO improve efficiency if meth lists are sorted
	for ifaceMethNum := range iface.NumMethods() {
		ifaceMeth := iface.Method(ifaceMethNum)
		numMethods := named.NumMethods()

		if numMethods == 0 {
			// typ implements iface using struct or interface embedding
			typ = named.Underlying()

			st, ok := typ.(*types.Struct)
			if !ok {
				_, ok = typ.(*types.Interface)
				return ok
			}

			for numField := range st.NumFields() {
				field := st.Field(numField)
				if field.Embedded() && p.hasHandwrittenImpl(field.Type(), iface) {
					return true
				}
			}

			return false // no field has handwritten iface impl
		}

		for typMethNum := range named.NumMethods() {
			typMeth := named.Method(typMethNum)

			if typMeth.Name() == ifaceMeth.Name() {
				typMethFile := p.fset.Position(typMeth.Pos()).Filename

				if _, ok := p.generated.Load(typMethFile); ok {
					return false
				}
			}
		}
	}

	return true
}
