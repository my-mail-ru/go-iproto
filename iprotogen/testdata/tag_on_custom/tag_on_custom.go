package tag_on_custom

type Custom struct{}

func (c Custom) MarshalIProto(buf []byte) ([]byte, error)    { return buf, nil }
func (c *Custom) UnmarshalIProto(buf []byte) ([]byte, error) { return buf, nil }

//adv:iproto:
type Outer struct {
	Field Custom `iproto:"ber"`
}
