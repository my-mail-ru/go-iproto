package iproto

type Marshaler interface {
	MarshalIProto([]byte) ([]byte, error)
}

type Unmarshaler interface {
	UnmarshalIProto([]byte) ([]byte, error)
}

type MarshalerUnmarshaler interface {
	Marshaler
	Unmarshaler
}
