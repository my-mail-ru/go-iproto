//go:build !integration
// +build !integration

package iproto

import (
	"encoding/hex"
	"errors"
	"testing"

	"github.com/google/go-cmp/cmp"
)

/*
perl -e 'for(my $i=7; $i < 64; $i+=7) {
    my $n=(1<<($i-7)) + int rand(1<<$i);
    printf qq({%d, "%s"},\n), $n, unpack H20=>pack w=>$n;
}'
*/

var berTests = []struct {
	n   uint64
	ber string
}{
	{108, "6c"},
	{3057, "9771"},
	{563874, "a2b522"},
	{72490434, "a2c8bb42"},
	{15203564508, "b8d1cff75c"},
	{639181971391, "92cd91dbbf3f"},
	{31450879175854, "8793abd3ecd92e"},
	{3816484303083008, "86e3e2a7f9b4ac00"},
	{6352014428907798528, "d893b8eeffd9d28000"},
}

func assert(t *testing.T, want, got any, fmtstr string, args ...any) {
	if diff := cmp.Diff(want, got); diff != "" {
		t.Errorf(fmtstr+" mismatch (-want +got):\n%s", append(args, diff)...)
	}
}

func TestEncodeBER(t *testing.T) {
	for _, tt := range berTests {
		got := EncodeBER(nil, tt.n)
		want, err := hex.DecodeString(tt.ber)

		if err != nil {
			t.Fatalf("%d, %s: %v", tt.n, tt.ber, err)
		}

		if diff := cmp.Diff(want, got); diff != "" {
			t.Errorf("EncodeBER(%d) mismatch (-want +got):\n%s", tt.n, diff)
		}
	}
}

func TestDecodeBER(t *testing.T) {
	for _, tt := range berTests {
		ber, err := hex.DecodeString(tt.ber)
		if err != nil {
			t.Fatalf("%d: hex.DecodeString(%s): %v", tt.n, tt.ber, err)
		}

		tail := []byte("junk")
		ber = append(ber, tail...) // дописываем мусор после числа, чтобы протестить получение длины

		got, gotTail, err := DecodeBER(ber)
		if err != nil {
			t.Fatalf("%d: DecodeBER(%s): %v", tt.n, tt.ber, err)
		}

		assert(t, tt.n, got, "DecodeBER(%s)", tt.ber)
		assert(t, tail, gotTail, "DecodeBER(%s): tail mismatch")
	}
}

func TestDecodeBERError(t *testing.T) {
	tests := []struct {
		ber     string
		wantErr error
	}{
		{"B199999999BAAAAAAAAAAD00", ErrBEROverflow},
		{"DEAD", ErrIncompleteBER},
	}

	for _, tt := range tests {
		ber, err := hex.DecodeString(tt.ber)
		if err != nil {
			t.Fatalf("hex.DecodeString(%s): %v", tt.ber, err)
		}

		_, _, gotErr := DecodeBER(ber)

		if !errors.Is(gotErr, tt.wantErr) {
			t.Errorf("DecodeBER(%s): unexpected error %v", tt.ber, gotErr)
		}
	}
}

func TestBERSize(t *testing.T) {
	for _, tt := range berTests {
		wantLen := len(tt.ber) / 2
		gotLen := BERSize(tt.n)

		assert(t, wantLen, gotLen, "BERSize(%d)", tt.n)
	}
}
