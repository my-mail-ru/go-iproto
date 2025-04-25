package iprototest

import (
	"github.com/my-mail-ru/go-iproto/tests/innerpkg"
	iprototypes "github.com/my-mail-ru/go-iproto/types"
)

//go:generate go run github.com/my-mail-ru/go-iproto/cmd/iprotogen

//adv:iproto:"ber"
type (
	MyInt    int
	UnixTime uint64
	UUID     [16]byte
)

type Embedded struct {
	EmbeddedInt    int `iproto:"ber"`
	EmbeddedString string
}

type Ints struct {
	IntField  MyInt              `iproto:"ber"`
	InnerInt  innerpkg.InnerInt  `iproto:"ber"`
	Int8      int8               //
	DefInt8   iprototypes.Int8   //
	Uint8     uint8              //
	Uint16    uint16             //
	Uint32    uint32             //
	Uint64    uint64             //
	DefUint8  iprototypes.Uint8  //
	DefUint16 iprototypes.Uint16 //
	DefUint32 iprototypes.Uint32 //
	DefUint64 iprototypes.Uint64 //
	Clipped   uint32             `iproto:"u16"`
}

//adv:iproto:
type MyStruct struct {
	Embedded
	Ints                  Ints
	String                string                         `iproto:"ber"`
	DefString             iprototypes.String             `iproto:"u8"`
	StrSlice              []string                       `iproto:"u16"`
	Timestamp             UnixTime                       //
	Float                 float32                        //
	DefFloat              iprototypes.Float32            //
	Double                float64                        //
	DefDouble             iprototypes.Float64            //
	NestedSlice           [][]string                     `iproto:"ber,u16,u8"`
	Bytes                 []byte                         `iproto:"ber"`
	DefBytes              iprototypes.Bytes              `iproto:"ber"`
	BER                   uint64                         `iproto:"ber"`
	DefBER                iprototypes.BER                `iproto:"ber"`
	PlainBoolT            bool                           //
	PlainBoolF            bool                           //
	CustomBoolT           bool                           `iproto:"true: 49, false: 48"`
	CustomBoolF           bool                           `iproto:"true: 49, false: 48"`
	CustomCharBoolT       bool                           `iproto:"true: 't', false: 'f'"`
	CustomCharBoolF       bool                           `iproto:"true: 't', false: 'f'"`
	IntArray              [4]int                         `iproto:"i32"`
	UUID                  UUID                           //
	MapStringString       map[string]string              //
	MapStringStringTagged map[string]string              `iproto:"ber,u8,u16"`
	Set                   map[string]struct{}            `iproto:"ber,ber"`
	MapIntInts            map[int][]Ints                 `iproto:"ber,i32,u8"`
	MapMapSlice           map[string]map[string][]string `iproto:"ber,u8,ber,u8,ber,u8"`
	MapSliceMap           map[string][]map[string]string `iproto:"ber,u8,ber,ber,u8,u8"`
}
