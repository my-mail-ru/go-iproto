package iprotogen

import (
	"go/ast"
	"strings"
	"testing"
)

func TestTypes(t *testing.T) {
	tests := []struct {
		in         Type
		required   []string
		disallowed []string
	}{
		{
			in:         Slice{LenType: Integer{Size: 0}, ElemType: Dumb{}},
			required:   []string{"iproto.EncodeBER", "for _, elemX := range x"},
			disallowed: []string{"append"},
		}, {
			in:         Slice{LenType: Integer{Size: 2, Max: 65535}, ElemType: Dumb{}},
			required:   []string{"fmt.Errorf", "for _, elemX := range x", "append"},
			disallowed: []string{"iproto.EncodeBER"},
		}, {
			in:         Integer{Size: 1},
			required:   []string{"append(buf, byte(x))"},
			disallowed: []string{">>", "iproto.EncodeBER"},
		}, {
			in:         Integer{Size: 4},
			required:   []string{"append(buf, byte(x)", ">>24"},
			disallowed: []string{"iproto.EncodeBER"},
		}, {
			in:         Float{Size: 4},
			required:   []string{"Float32bits", ">>24"},
			disallowed: []string{"Float64bits"},
		}, {
			in:         Float{Size: 8},
			required:   []string{"Float64bits", ">>56"},
			disallowed: []string{"Float32bits"},
		}, {
			in:         StringOrBytes{LenType: Integer{Size: 0}},
			required:   []string{"append(buf, x...)"},
			disallowed: []string{"for _"},
		}, {
			in: Struct{Fields: []StructField{
				{
					Ident: ast.NewIdent("IntField"),
					Type:  Integer{Size: 4},
				}, {
					Ident: ast.NewIdent("StringField"),
					Type:  StringOrBytes{LenType: Integer{Size: 4}},
				}, {
					Ident: ast.NewIdent("StringPtr"),
					Type: Pointer{
						Type: StringOrBytes{LenType: Integer{Size: 4}},
					},
				}}},
			required:   []string{"x.IntField>>", "x.StringField...", "(*x.StringPtr)..."},
			disallowed: []string{"iproto.EncodeBER"},
		}, {
			in:         Custom{hasMarshaler: true, hasUnmarshaler: true},
			required:   []string{"x.MarshalIProto(buf)", "if err != nil"},
			disallowed: []string{">>"},
		},
	}

	x := ast.NewIdent("x")

	for _, tt := range tests {
		src := astToSource(tt.in.EmitMarshaler(x, nil))

		for _, s := range tt.required {
			if !strings.Contains(src, s) {
				t.Fatalf("%#v\ngenerated source should contain %q, but it doesn't:\n%s\n", tt.in, s, src)
			}
		}

		for _, s := range tt.disallowed {
			if strings.Contains(src, s) {
				t.Fatalf("%#v\ngenerated source should not contain %q, but it does:\n%s\n", tt.in, s, src)
			}
		}
	}
}
