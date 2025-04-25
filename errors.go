package iproto

import (
	"errors"
)

// Ошибки
var (
	ErrSizeMismatch = errors.New("length mismatch while decoding for type")
	ErrOverflow     = errors.New("overflow")
)
