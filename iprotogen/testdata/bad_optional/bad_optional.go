package bad_optional

//adv:iproto:
type BadStruct struct {
	Field int `iproto:"optional,ber"`
}
