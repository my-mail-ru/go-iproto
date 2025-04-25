package iproto

import "errors"

// Ошибки [DecodeBER]
var (
	ErrBEROverflow   = errors.New("BER: 64 bit overflow")
	ErrIncompleteBER = errors.New("BER: unexpected end of data")
)

// EncodeBER - упаковка инта в формат произвольной длины, совместимый с perl pack 'w' (aka ULEB128).
//
// Списано с небольшими правками из go-iproto/sbox
func EncodeBER(buf []byte, n uint64) []byte {
	var l int

	switch {
	case n < 1<<7:
		return append(buf, byte(n))

	case n < 1<<14:
		return append(buf, byte(0x80|(n>>7)), byte(n&0x7F))

	case n < 1<<21:
		return append(buf, byte(0x80|(n>>14)), byte(0x80|(n>>7)), byte(n&0x7F))

	case n < 1<<28:
		l = 4

	case n < 1<<35:
		l = 5

	case n < 1<<42:
		l = 6

	case n < 1<<49:
		l = 7

	case n < 1<<56:
		l = 8

	case n < 1<<63:
		l = 9

	default:
		l = 10
	}

	for i := l - 1; i > 0; i-- {
		buf = append(buf, byte(0x80|(n>>(7*i))))
	}

	return append(buf, byte(n&0x7F))
}

// DecodeBER - распаковка инта из формата произвольной длины, совместимого с perl unpack 'w' (aka ULEB128).
// tail - остаток буфера после распаковки.
//
// Списано с небольшими правками из go-iproto/sbox
func DecodeBER(ber []byte) (res uint64, tail []byte, err error) {
	for i, b := range ber {
		res = (res << 7) | uint64(b&0x7F)

		if b < 0x80 {
			return res, ber[i+1:], nil
		} else if res > (1<<64-1)>>7 {
			return 0, ber[i+1:], ErrBEROverflow
		}
	}

	return 0, nil, ErrIncompleteBER // XXX корректно возвращать ber[len(ber):], но всё равно эти данные уже не пригодятся
}

// BERSize возвращает кол-во байт, необходимое для кодирования числа n.
func BERSize(n uint64) int {
	l := 1

	for ; n > 1<<7; l++ {
		n >>= 7
	}

	return l
}
