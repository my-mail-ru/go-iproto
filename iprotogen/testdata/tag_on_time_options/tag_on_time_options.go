package tag_on_time_options

import "time"

//adv:iproto:
type Outer struct {
	Field time.Time `iproto:"u32,extra"`
}
