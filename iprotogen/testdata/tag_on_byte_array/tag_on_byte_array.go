package tag_on_byte_array

//adv:iproto:
type Outer struct {
	Field [16]byte `iproto:"u8,extra"`
}
