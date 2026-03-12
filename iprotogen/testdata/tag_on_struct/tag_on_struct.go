package tag_on_struct

type Inner struct {
	ID int `iproto:"i32"`
}

//adv:iproto:
type Outer struct {
	Field Inner `iproto:"ber"`
}
