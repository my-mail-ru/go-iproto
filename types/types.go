package iprototypes

import "github.com/my-mail-ru/go-iproto"

//go:generate go run github.com/my-mail-ru/go-iproto/cmd/iprotogen

//adv:iproto:
type (
	Uint8   uint8
	Uint16  uint16
	Uint32  uint32
	Uint64  uint64
	Int8    int8
	Int16   int16
	Int32   int32
	Int64   int64
	Float32 float32
	Float64 float64
)

//adv:iproto:"ber"
type (
	BER    uint64
	String string
	Bytes  []uint8
	Slice  []iproto.MarshalerUnmarshaler
)

type (
	Byte = Uint8
	Rune = Uint32
)
