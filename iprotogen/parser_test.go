package iprotogen

import (
	"strings"
	"testing"
)

func TestOptionalOnWrongType(t *testing.T) {
	p, err := NewParser(WithParseTests)
	if err != nil {
		t.Fatal(err)
	}

	_, err = p.ParsePackage("github.com/my-mail-ru/go-iproto/iprotogen/testdata/bad_optional")
	if err == nil {
		t.Fatal("expected error for optional tag on non-pointer type, got nil")
	}

	if !strings.Contains(err.Error(), "optional") {
		t.Fatalf("expected error to mention \"optional\", got: %s", err)
	}
}
