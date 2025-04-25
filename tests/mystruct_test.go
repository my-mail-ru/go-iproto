package iprototest

import (
	"encoding/hex"
	"errors"
	"testing"
	"unsafe"

	"github.com/google/go-cmp/cmp"

	iproto "github.com/my-mail-ru/go-iproto"
	iprototypes "github.com/my-mail-ru/go-iproto/types"
)

func TestMyStructMarshalUnmarshal(t *testing.T) {
	orig := MyStruct{
		Embedded: Embedded{
			EmbeddedInt:    12345,
			EmbeddedString: "test",
		},
		Ints: Ints{
			IntField:  123456789,
			InnerInt:  456789,
			Int8:      0x5A,
			DefInt8:   111,
			Uint8:     222,
			Uint16:    33333,
			Uint32:    44444444,
			Uint64:    5555555555555555,
			DefUint8:  234,
			DefUint16: 34567,
			DefUint32: 4567890,
			DefUint64: 5678901234,
			Clipped:   65533,
		},
		String:                "test",
		DefString:             "Test",
		StrSlice:              []string{"foo", "bar"},
		Timestamp:             1733506469,
		Float:                 3.14,
		DefFloat:              2.7,
		Double:                3.14159265358979323846264338327950288419716939937510582097494459,
		DefDouble:             2.718281828459045235360287471352,
		Bytes:                 []byte("test1234"),
		DefBytes:              iprototypes.Bytes("zxcvbnm"),
		BER:                   0x9999888877776666,
		DefBER:                0x1111222233334444,
		PlainBoolT:            true,
		PlainBoolF:            false,
		CustomBoolT:           true,
		CustomBoolF:           false,
		CustomCharBoolT:       true,
		CustomCharBoolF:       false,
		IntArray:              [4]int{1, -2, 3, -4},
		UUID:                  [16]byte{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15},
		MapStringString:       map[string]string{"test": "12345", "foobar": "baz"},
		MapStringStringTagged: map[string]string{"foo": "12312312312", "bar": "1234"},
		Set:                   map[string]struct{}{"one": struct{}{}, "two": struct{}{}},
		MapIntInts: map[int][]Ints{
			1: []Ints{
				{
					IntField: MyInt(1),
					Int8:     1,
				},
				{
					IntField: MyInt(2),
					Int8:     2,
				},
			},
			-1: []Ints{{IntField: MyInt(-1), Int8: -1}},
		},
		MapMapSlice: map[string]map[string][]string{
			"foo": {
				"bar": {"b", "a", "z"},
				"qux": {"x", "y", "z"},
			},
			"key": {
				"valkey": {"1", "2", "3", "4"},
				"keyval": {"vals", "strs"},
			},
		},
		MapSliceMap: map[string][]map[string]string{
			"foo": {
				{
					"k1":   "v1",
					"key2": "val2",
				},
				{
					"key1": "val1",
					"k2":   "v2",
				},
			},
		},
		NestedSlice: [][]string{
			{"who", "cares"},
			{"a", "b", "c", "d"},
		},
	}

	bytes, err := orig.MarshalIProto(nil)
	if err != nil {
		t.Fatal(err)
	}

	t.Log("\n" + hex.Dump(bytes))

	var got MyStruct

	if _, err = got.UnmarshalIProto(bytes); err != nil {
		t.Fatal(err)
	}

	if diff := cmp.Diff(orig, got); diff != "" {
		t.Fatal("unexpected difference (-want +got)\n", diff)
	}
}

func TestBoundCheck(t *testing.T) {
	_, err := MyStruct{Ints: Ints{Clipped: 123456}}.MarshalIProto(nil)

	if !errors.Is(err, iproto.ErrOverflow) {
		t.Errorf("got %v, want iproto.ErrOverflow", err)
	}
}

func TestSliceLeak(t *testing.T) {
	data := MyStruct{
		String:    "test",
		DefString: "Test",
		Bytes:     []byte("test1234"),
		DefBytes:  iprototypes.Bytes("zxcv9876"),
	}

	buf, err := data.MarshalIProto(nil)
	if err != nil {
		t.Fatal("MarshalIProto:", err)
	}

	var got MyStruct
	if _, err = got.UnmarshalIProto(buf); err != nil {
		t.Fatal("UnmarshalIProto:", err)
	}

	if isPtrInSlice(buf, unsafe.SliceData(got.Bytes)) {
		t.Fatal("Bytes field leaks buf's underlying array")
	}

	if isPtrInSlice(buf, unsafe.SliceData(got.DefBytes)) {
		t.Fatal("DefBytes field leaks buf's underlying array")
	}

	if isPtrInSlice(buf, stringData(got.String)) {
		t.Fatal("String field leaks buf's underlying array")
	}

	if isPtrInSlice(buf, stringData(got.DefString)) {
		t.Fatal("DefString field leaks buf's underlying array")
	}
}

func isPtrInSlice(sl []byte, ptr *byte) bool {
	start := uintptr(unsafe.Pointer(unsafe.SliceData(sl)))
	end := start + uintptr(cap(sl))
	uptr := uintptr(unsafe.Pointer(ptr))

	return start <= uptr && uptr < end
}

type stringHeader struct {
	data   *byte
	length int
}

// like unsafe.StringData but works with defined types without explicit type conversion (which may result in copying)
func stringData[S ~string](s S) *byte {
	return (*stringHeader)(unsafe.Pointer(&s)).data
}
