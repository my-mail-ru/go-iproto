package iprototest

import (
	"database/sql"
	"time"

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

type EventType string
type myTime = time.Time

type Event[T any] struct {
	Data T
	Type EventType `iproto:"ber"`
}

type Pair[A, B any] struct {
	First  A
	Second B
}

//adv:iproto:
type MyStruct struct {
	Embedded
	Ints                  Ints
	String                string                         `iproto:"ber"`
	DefString             iprototypes.String             `iproto:"u8"`
	DflString             string                         //
	SignedLenString       string                         `iproto:"i8"`
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
	DefBoolT              iprototypes.Bool               //
	DefBoolF              iprototypes.Bool               //
	IntArray              [4]int                         `iproto:"i32"`
	UUID                  UUID                           //
	MapStringString       map[string]string              //
	MapStringStringTagged map[string]string              `iproto:"ber,u8,u16"`
	Set                   map[string]struct{}            `iproto:"ber,ber"`
	MapIntInts            map[int][]Ints                 `iproto:"ber,i32,u8"`
	MapMapSlice           map[string]map[string][]string `iproto:"ber,u8,ber,u8,ber,u8"`
	MapSliceMap           map[string][]map[string]string `iproto:"ber,u8,ber,ber,u8,u8"`
	IntPtr                *int                           `iproto:"ber"`
	StructPtr             *Ints                          //
	GenericComplex        Event[Ints]                    //
	GenericSimple         Event[string]                  `iproto:"u16"`
	GenericSlice          []Event[string]                `iproto:"u16,u8"`
	GenericPair           Pair[string, int32]            `iproto:"u8,i16"`
	GenericPairComplex    Pair[[]string, int32]          `iproto:"ber,u8,i16"`
	TimeNano              time.Time                      //
	TimeNanoExplicit      time.Time                      `iproto:"i64"`
	TimeUnix              time.Time                      `iproto:"u32"`
	TimeAlias             myTime                         //
	// Optional pointer types
	OptionalInt       *int    `iproto:"optional,ber"`
	OptionalIntNil    *int    `iproto:"optional,ber"`
	OptionalStr       *string `iproto:"optional,u8"`
	OptionalStrNil    *string `iproto:"optional,u8"`
	OptionalStruct    *Ints   `iproto:"optional"`
	OptionalStructNil *Ints   `iproto:"optional"`
	// Optional sql.Null types
	OptionalNullStr      sql.NullString    `iproto:"optional,ber"`
	OptionalNullStrEmpty sql.NullString    `iproto:"optional,ber"`
	OptionalNullInt64    sql.NullInt64     `iproto:"optional"`
	OptionalNullInt64Nil sql.NullInt64     `iproto:"optional"`
	OptionalNullBool     sql.NullBool      `iproto:"optional"`
	OptionalNullFloat64  sql.NullFloat64   `iproto:"optional"`
	OptionalNullInt32    sql.NullInt32     `iproto:"optional"`
	OptionalNullInt16    sql.NullInt16     `iproto:"optional"`
	OptionalNullByte     sql.NullByte      `iproto:"optional"`
	OptionalNullTime     sql.NullTime      `iproto:"optional"`
	OptionalNullTimeU32  sql.NullTime      `iproto:"optional,u32"`
	OptionalNullGeneric  sql.Null[int64]   `iproto:"optional"`
	OptionalNullFloat32  sql.Null[float32] `iproto:"optional"`
	// Backwards compatibility: non-optional pointer (existing behavior)
	// IntPtr and StructPtr already test this above
	// Backwards compatibility: sql.Null without optional tag (parsed as struct)
	NonOptionalNull sql.NullString //
}
