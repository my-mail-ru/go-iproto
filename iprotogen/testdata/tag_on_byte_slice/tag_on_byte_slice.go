package tag_on_byte_slice

//adv:iproto:
type Outer struct {
	Field []byte `iproto:"ber,u8,extra"`
}
