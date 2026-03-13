package iprotogen

import (
	"strings"
	"testing"
)

func TestUnsupportedTags(t *testing.T) {
	tests := []struct {
		name    string
		pkg     string
		wantErr string
	}{
		{
			name:    "optional on non-pointer type",
			pkg:     "github.com/my-mail-ru/go-iproto/iprotogen/testdata/bad_optional",
			wantErr: "optional",
		},
		{
			name:    "tag on non-generic struct field",
			pkg:     "github.com/my-mail-ru/go-iproto/iprotogen/testdata/tag_on_struct",
			wantErr: "non-generic struct",
		},
		{
			name:    "tag on custom marshaler type",
			pkg:     "github.com/my-mail-ru/go-iproto/iprotogen/testdata/tag_on_custom",
			wantErr: "tags are not supported",
		},
		{
			name:    "extra options on time.Time",
			pkg:     "github.com/my-mail-ru/go-iproto/iprotogen/testdata/tag_on_time_options",
			wantErr: "unsupported tag options for time.Time",
		},
		{
			name:    "extra options on byte array",
			pkg:     "github.com/my-mail-ru/go-iproto/iprotogen/testdata/tag_on_byte_array",
			wantErr: "unsupported tag options for byte array",
		},
		{
			name:    "extra options on byte slice",
			pkg:     "github.com/my-mail-ru/go-iproto/iprotogen/testdata/tag_on_byte_slice",
			wantErr: "unsupported tag options for byte slice",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p, err := NewParser(WithParseTests)
			if err != nil {
				t.Fatal(err)
			}

			_, err = p.ParsePackage(tt.pkg)
			if err == nil {
				t.Fatal("expected error, got nil")
			}

			if !strings.Contains(err.Error(), tt.wantErr) {
				t.Fatalf("expected error containing %q, got: %s", tt.wantErr, err)
			}
		})
	}
}
