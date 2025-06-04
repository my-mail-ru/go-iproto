// iprotogen - iproto serialization code generator.
//
// Flags:
//
//	-in            - input file name (default: $GOFILE)
//	-tags          - build tags (go -tags or golangci-lint --build-tags)
//	-only          - process only this file (default: process all *.go files in the folder)
//	-r             - process subpackages recursively
//	-no-format     - disable goimports formatting
//	-stdout        - print results on stdout (default: write _generated.go files)
//	-ignore-gen    - completely ignore generated files
//	-tests         - process *_test.go files
//	-disable-strip - disable copying the source code to the temp dir with func bodies removed
//	-keep-stripped - don't remove temporary directory containing stripped code
package main

import (
	"errors"
	"flag"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/my-mail-ru/go-iproto/iprotogen"
)

var (
	flagInFile            = flag.String("in", os.Getenv("GOFILE"), "input file (default: $GOFILE)")
	flagBuildTags         = flag.String("tags", "", "build tags")
	flagOnlyFile          = flag.Bool("only", false, "process the specified only (default: process the whole package)")
	flagRecurse           = flag.Bool("r", false, "process subpackages recursively")
	flagDisableFormatting = flag.Bool("no-format", false, "disable output formatting")
	flagStdout            = flag.Bool("stdout", false, "print results on stdout instead of writing file(s)")
	flagIgnoreGenerated   = flag.Bool("ignore-gen", false, "completely ignore generated files")
	flagTests             = flag.Bool("tests", false, "process *_test.go files")
	flagDisableStrip      = flag.Bool("disable-strip", false, "disable copying stripped code to temporary dir")
	flagKeepStripped      = flag.Bool("keep-stripped", false, "don't remove temporary directory containing stripped code")
)

func main() {
	log.SetFlags(0)

	if err := start(); err != nil {
		log.Fatal(err)
	}
}

func start() (retErr error) {
	flag.Parse()

	if *flagInFile == "" {
		return errors.New("`-in`/`$GOFILE` is mantatory")
	}

	inFileAbs, err := filepath.Abs(*flagInFile)
	if err != nil {
		return fmt.Errorf("%s: can't get absolute path: %w", *flagInFile, err)
	}

	origSrcDir := filepath.Dir(inFileAbs)
	srcDir := origSrcDir
	parserOpt, generatorOpt, writeFile := processFlags(inFileAbs)

	if !*flagDisableStrip {
		modDir := ""

		modDir, srcDir, err = iprotogen.StripPackage(srcDir, *flagTests)

		defer func() {
			retErr = removeTempDir(modDir, *flagKeepStripped, retErr)
		}()

		if err != nil {
			return err
		}

		if err = os.Chdir(srcDir); err != nil {
			return fmt.Errorf("chdir %s: %w", srcDir, err)
		}

		parserOpt = append(parserOpt, iprotogen.WithModuleDir(modDir))
	}

	srcPkg := srcDir
	if *flagRecurse {
		srcPkg += "/..."
	}

	parser, err := iprotogen.NewParser(parserOpt...)
	if err != nil {
		return fmt.Errorf("%s: %w", inFileAbs, err)
	}

	files, err := parser.ParsePackage(srcPkg)
	if err != nil {
		return err // already contains the source info
	}

	for fname, data := range files {
		if len(data.Decls) == 0 {
			continue
		}

		content, err := data.Emit(generatorOpt...)
		if err != nil {
			return fmt.Errorf("%s: %w", fname, err)
		}

		fname, err = iprotogen.RebasePath(srcDir, origSrcDir, iprotogen.GeneratedFileName(fname))
		if err != nil {
			return err
		}

		if err = writeFile(fname, content); err != nil {
			return err
		}
	}

	return nil
}

func processFlags(inFileAbs string) (parserOpt []iprotogen.ParserOptionsFunc, generatorOpt []iprotogen.FileEmitOptionsFunc, writeFile func(fname string, content []byte) error) {
	if *flagBuildTags != "" {
		parserOpt = append(parserOpt, iprotogen.WithBuildFlag("-tags="+*flagBuildTags))
	}

	if *flagOnlyFile {
		parserOpt = append(parserOpt, iprotogen.WithOnlyFile(inFileAbs))
	}

	if *flagIgnoreGenerated {
		parserOpt = append(parserOpt, iprotogen.WithIgnoreGenerated)
	}

	if *flagTests {
		parserOpt = append(parserOpt, iprotogen.WithParseTests)
	}

	if *flagDisableFormatting {
		generatorOpt = append(generatorOpt, iprotogen.WithDisableFormatting)
	}

	writeFile = func(fname string, content []byte) error {
		if err := os.WriteFile(fname, content, 0o644); err != nil { //nolint:gosec
			return fmt.Errorf("error writing generated code: %w", err)
		}

		log.Println("writing", fname)

		return nil
	}

	if *flagStdout {
		writeFile = func(fname string, content []byte) error {
			_, err := fmt.Printf("//////// %s ////////\n%s\n\n", fname, content)
			return err
		}
	}

	return
}

func removeTempDir(modDir string, keepStripped bool, retErr error) error {
	if modDir == "" {
		return retErr
	}

	tmpModDirPrefix := string(filepath.Separator) + iprotogen.TmpDirPrefix
	if !strings.Contains(modDir, tmpModDirPrefix) {
		return errors.New("internal error: temporary module directory path " + modDir + " doesn't contain " + tmpModDirPrefix)
	}

	if keepStripped {
		log.Println("stripped module directory:", modDir)
		return retErr
	}

	if err := os.RemoveAll(modDir); err != nil {
		err = fmt.Errorf("error removing temporary dir %s: %w", modDir, err)

		if retErr == nil {
			return err
		}

		return errors.Join(retErr, err)
	}

	return retErr
}
