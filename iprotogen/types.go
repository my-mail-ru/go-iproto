package iprotogen

import (
	"go/ast"
	"go/token"
	"strconv"
)

type Type interface {
	EmitMarshaler(ast.Expr, []ast.Stmt) []ast.Stmt
	EmitUnmarshaler(ast.Expr, []ast.Stmt) []ast.Stmt
}

type Slice struct {
	LenType  Integer  // LenType.TypeExpr should always be int
	TypeExpr ast.Expr // type of the slice itself, not an element type
	ElemType Type
}

func (sl Slice) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	iterVar := varFromExpr(x, "elem")
	body := sl.ElemType.EmitMarshaler(iterVar, newBlock())

	if body == nil {
		return nil
	}

	block = emitLenMarshal(x, sl.LenType, block)

	return append(block, &ast.RangeStmt{
		Key:   identBlank,
		Value: iterVar,
		X:     x,
		Tok:   token.DEFINE,
		Body:  &ast.BlockStmt{List: body},
	})
}

func (sl Slice) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	counterVar := varFromExpr(x, "i")
	body := sl.ElemType.EmitUnmarshaler(&ast.IndexExpr{
		X:     x,
		Index: counterVar,
	}, newBlock())

	if body == nil {
		return nil
	}

	lenVar := varFromExpr(x, "len")
	block = sl.LenType.EmitUnmarshaler(lenVar, block)

	return append(
		emitVarAssign(
			x,
			exprCall(identMake, sl.TypeExpr, lenVar),
			emitBoundCheck(exprLenBuf, token.LSS, lenVar, false, astToSourceUpper(x), block), // anti-DoS check
		),
		stmtFor(counterVar, lenVar, body),
	)
}

type Array struct {
	Len      *ast.BasicLit
	ElemType Type
}

func (arr Array) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	iterVal := varFromExpr(x, "elem")
	body := arr.ElemType.EmitMarshaler(iterVal, newBlock())

	if body == nil {
		return nil
	}

	return append(block, &ast.RangeStmt{
		Key:   identBlank,
		Value: iterVal,
		X:     x,
		Tok:   token.DEFINE,
		Body:  &ast.BlockStmt{List: body},
	})
}

func (arr Array) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	counterVar := varFromExpr(x, "i")
	body := arr.ElemType.EmitUnmarshaler(&ast.IndexExpr{
		X:     x,
		Index: counterVar,
	}, newBlock())

	if body == nil {
		return nil
	}

	return append(
		emitBoundCheck(exprLenBuf, token.LSS, arr.Len, false, astToSourceUpper(x), block), // anti-DoS check
		stmtFor(counterVar, arr.Len, body),
	)
}

type Map struct {
	LenType   Integer  // LenType.TypeExpr should always be int
	TypeExpr  ast.Expr // type of the map itself
	KeyType   Type
	ValueType Type
}

func (m Map) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	keyVar := varFromExpr(x, "key")
	valVar := varFromExpr(x, "val")

	body := m.KeyType.EmitMarshaler(keyVar, newBlock())
	if body == nil {
		return nil
	}

	valBody := m.ValueType.EmitMarshaler(valVar, body)
	if valBody == nil {
		return nil
	}

	valExpr := ast.Expr(nil)

	// don't emit value iterator var for an empty value
	if len(valBody) != len(body) {
		valExpr = valVar
	}

	block = emitLenMarshal(x, m.LenType, block)

	return append(block, &ast.RangeStmt{
		Key:   keyVar,
		Value: valExpr,
		X:     x,
		Tok:   token.DEFINE,
		Body:  &ast.BlockStmt{List: valBody},
	})
}

func (m Map) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	keyVar := varFromExpr(x, "key")

	body := m.KeyType.EmitUnmarshaler(keyVar, newBlock())
	if body == nil {
		return nil
	}

	body = m.ValueType.EmitUnmarshaler(&ast.IndexExpr{
		X:     x,
		Index: keyVar,
	}, body)
	if body == nil {
		return nil
	}

	counterVar := varFromExpr(x, "i")
	lenVar := varFromExpr(x, "len")
	block = m.LenType.EmitUnmarshaler(lenVar, block)

	return append(
		emitVarAssign(
			x,
			exprCall(identMake, m.TypeExpr, lenVar),
			emitBoundCheck(exprLenBuf, token.LSS, lenVar, false, astToSourceUpper(x), block), // anti-DoS check
		),
		stmtFor(counterVar, lenVar, body),
	)
}

type Integer struct {
	TypeExpr ast.Expr
	Size     int
	Min      int64
	Max      uint64
}

func (i Integer) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	if i.Size == 0 {
		u64 := x
		if !isUint64(i.TypeExpr) {
			u64 = exprCall(identUint64, x)
		}

		return append(block, stmtAssign(identBuf, exprCall(exprEncodeBER, identBuf, u64)))
	}

	if i.Min != 0 {
		block = emitBoundCheck(x, token.LSS, litInt64(i.Min), true, astToSourceUpper(x), block)
	}

	if i.Max != 0 {
		block = emitBoundCheck(x, token.GTR, litUint64(i.Max), true, astToSourceUpper(x), block)
	}

	appendArgs := []ast.Expr{identBuf}

	for b := range i.Size {
		operand := x
		if b != 0 {
			operand = &ast.BinaryExpr{
				Op: token.SHR,
				X:  x,
				Y:  litInt(b * 8),
			}
		}

		if !isUint8(i.TypeExpr) {
			operand = exprCall(identByte, operand)
		}

		appendArgs = append(appendArgs, operand)
	}

	return emitAppendBuf(appendArgs, token.NoPos, block)
}

func (i Integer) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	if i.Size == 0 {
		isU64 := isUint64(i.TypeExpr)
		u64 := x

		if !isU64 {
			u64 = identU64
		}

		block = append(
			block,
			&ast.AssignStmt{
				Tok: token.ASSIGN,
				Lhs: []ast.Expr{u64, identBuf, identErr},
				Rhs: []ast.Expr{exprCall(exprDecodeBER, identBuf)},
			},
			stmtErrCheck(),
		)

		if isU64 {
			return block
		}

		return emitVarAssign(x, exprCall(i.TypeExpr, identU64), block)
	}

	block = emitBoundCheck(exprLenBuf, token.LSS, litInt(i.Size), false, astToSourceUpper(x), block)

	expr := ast.Expr(exprBuf0) // x = Type(buf[0])

	if i.Size > 1 {
		// x = Type(binary.Uint$BITSIZE(buf))
		expr = exprCall(exprDot(exprLittleEndian, ast.NewIdent("Uint"+strconv.Itoa(8*i.Size))), identBuf)
	}

	if i.Min < 0 {
		expr = exprCall(ast.NewIdent("int"+strconv.Itoa(8*i.Size)), expr)
	}

	// valid for all sizes including 1
	if !isUintN(i.TypeExpr, i.Size) {
		expr = exprCall(i.TypeExpr, expr)
	}

	return append(
		emitVarAssign(x, expr, block),
		stmtBufRewind(litInt(i.Size)),
	)
}

type Float struct {
	TypeExpr ast.Expr
	Size     int
}

func (f Float) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	bitsVar := varFromExpr(x, "bits")
	bitsFunc := exprFloat64bits
	typeExpr := identFloat64

	if f.Size == 4 {
		bitsFunc = exprFloat32bits
		typeExpr = identFloat32
	}

	val := x
	if !isIdent(f.TypeExpr, typeExpr.Name) {
		val = exprCall(typeExpr, val)
	}

	block = append(block, stmtDefine(bitsVar, exprCall(bitsFunc, val)))

	return Integer{Size: f.Size}.EmitMarshaler(bitsVar, block)
}

func (f Float) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	bitsVar := varFromExpr(x, "bits")
	bitsFunc := exprFloat64frombits
	typeExpr := identUint64
	floatIdent := identFloat64

	if f.Size == 4 {
		bitsFunc = exprFloat32frombits
		typeExpr = identUint32
		floatIdent = identFloat32
	}

	val := exprCall(bitsFunc, bitsVar)
	if !isIdent(f.TypeExpr, floatIdent.Name) {
		val = exprCall(f.TypeExpr, val)
	}

	return emitVarAssign(
		x,
		val,
		Integer{Size: f.Size, TypeExpr: typeExpr}.EmitUnmarshaler(bitsVar, block),
	)
}

// Bool represents a value with an underlying type bool.
// TypeExpr is the Go type expression (e.g. identBool for bool, or a defined type like MyBool).
// When decoding, False byte is decoded as false, and anything else as true (not only the True byte).
type Bool struct {
	TypeExpr ast.Expr
	True     *ast.BasicLit
	False    *ast.BasicLit
}

func (b Bool) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	return append(block, &ast.IfStmt{
		Cond: x,
		Body: &ast.BlockStmt{
			List: emitAppendBuf([]ast.Expr{identBuf, b.True}, token.NoPos, nil),
		},
		Else: &ast.BlockStmt{
			List: emitAppendBuf([]ast.Expr{identBuf, b.False}, token.NoPos, nil),
		},
	})
}

func (b Bool) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	block = emitBoundCheck(exprLenBuf, token.LSS, lit1, false, astToSourceUpper(x), block)

	val := ast.Expr(&ast.BinaryExpr{
		X:  exprBuf0,
		Op: token.NEQ,
		Y:  b.False,
	})

	if !isIdent(b.TypeExpr, "bool") {
		val = exprCall(b.TypeExpr, val)
	}

	block = emitVarAssign(x, val, block)

	return append(block, stmtBufRewind(lit1))
}

type StringOrBytes struct {
	LenType  Integer
	TypeExpr ast.Expr
	IsSlice  bool
}

func (s StringOrBytes) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	block = emitLenMarshal(x, s.LenType, block)
	return emitAppendBuf([]ast.Expr{identBuf, x}, 1, block)
}

func (s StringOrBytes) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	lenVar := varFromExpr(x, "len")
	block = s.LenType.EmitUnmarshaler(lenVar, block)
	block = emitBoundCheck(exprLenBuf, token.LSS, lenVar, false, astToSourceUpper(x), block)

	subslice := ast.Expr(&ast.SliceExpr{X: identBuf, High: lenVar})

	// strings are always copied from byte slices
	if s.IsSlice {
		subslice = exprCallVarargs(identAppend, &ast.CompositeLit{Type: exprBytes}, subslice)
	}

	if !isByteSlice(s.TypeExpr) {
		subslice = exprCall(s.TypeExpr, subslice)
	}

	block = emitVarAssign(x, subslice, block)

	return append(block, stmtBufRewind(lenVar))
}

type ByteArray struct {
	Len      *ast.BasicLit
	TypeExpr ast.Expr
}

func (ByteArray) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	return emitAppendBuf([]ast.Expr{identBuf, &ast.SliceExpr{X: x}}, 1, block)
}

func (ba ByteArray) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	block = emitBoundCheck(exprLenBuf, token.LSS, ba.Len, false, astToSourceUpper(x), block)

	block = emitVarAssign(x, exprCall(ba.TypeExpr, &ast.SliceExpr{
		X:    identBuf,
		High: ba.Len,
	}), block)

	return append(block, stmtBufRewind(ba.Len))
}

type StructField struct {
	Ident *ast.Ident
	Type  Type
}

type Struct struct {
	Fields []StructField
}

func (s Struct) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	for _, field := range s.Fields {
		block = field.Type.EmitMarshaler(exprDot(x, field.Ident), block)
		if block == nil {
			return nil
		}
	}

	return block
}

func (s Struct) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	for _, field := range s.Fields {
		block = field.Type.EmitUnmarshaler(fieldSelector(x, field.Ident), block)
		if block == nil {
			return nil
		}
	}

	return block
}

// StructLiteral is used for map keys and values
type StructLiteral struct {
	Fields   []StructField
	TypeExpr ast.Expr
}

func (s StructLiteral) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	return Struct{Fields: s.Fields}.EmitMarshaler(x, block)
}

func (s StructLiteral) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	fieldExprs := make([]ast.Expr, len(s.Fields))

	for i, field := range s.Fields {
		fieldVar := varFromExpr(x, "field"+field.Ident.Name)
		block = field.Type.EmitUnmarshaler(fieldVar, block)
		fieldExprs[i] = &ast.KeyValueExpr{
			Key:   field.Ident,
			Value: fieldVar,
		}
	}

	return emitVarAssign(x, &ast.CompositeLit{
		Type: s.TypeExpr,
		Elts: fieldExprs,
	}, block)
}

type Pointer struct {
	Type     Type
	TypeExpr ast.Expr // the type a pointer is pointing to
}

func (p Pointer) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	return p.Type.EmitMarshaler(&ast.ParenExpr{
		X: &ast.StarExpr{
			X: x,
		},
	}, block)
}

func (p Pointer) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	block = emitVarAssign(x, exprCall(identNew, p.TypeExpr), block)
	return p.Type.EmitUnmarshaler(&ast.StarExpr{X: x}, block)
}

type Custom struct {
	hasMarshaler   bool
	hasUnmarshaler bool
}

func (c Custom) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	if !c.hasMarshaler {
		return nil
	}

	return append(block,
		&ast.AssignStmt{
			Tok: token.ASSIGN,
			Lhs: exprBufErr,
			Rhs: []ast.Expr{exprCall(exprDot(x, identMarshalIProto), identBuf)},
		},
		stmtErrCheck(),
	)
}

func (c Custom) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	if !c.hasUnmarshaler {
		return nil
	}

	return append(block,
		&ast.AssignStmt{
			Tok: token.ASSIGN,
			Lhs: exprBufErr,
			Rhs: []ast.Expr{exprCall(fieldSelector(x, identUnmarshalIProto), identBuf)},
		},
		stmtErrCheck(),
	)
}

// TimeUnixNanoI64 serializes time.Time as nanoseconds since the Unix epoch in int64.
// Range: 1677-09-21T00:12:43Z to 2262-04-11T23:47:16Z.
type TimeUnixNanoI64 struct{}

func (TimeUnixNanoI64) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	nanoVar := varFromExpr(x, "nano")
	block = append(block, stmtDefine(nanoVar, exprCall(exprDot(x, identUnixNano))))

	return Integer{TypeExpr: identInt64, Size: 8}.EmitMarshaler(nanoVar, block)
}

func (TimeUnixNanoI64) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	nanoVar := varFromExpr(x, "nano")
	block = Integer{TypeExpr: identInt64, Size: 8}.EmitUnmarshaler(nanoVar, block)

	return emitVarAssign(x, exprCall(exprTimeUnix, lit0, nanoVar), block)
}

// TimeUnixU32 serializes time.Time as seconds since the Unix epoch in uint32.
// Range: 1970-01-01T00:00:00Z to 2106-02-07T06:28:15Z.
type TimeUnixU32 struct{}

func (TimeUnixU32) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	secVar := varFromExpr(x, "sec")
	block = append(block, stmtDefine(secVar, exprCall(identUint32, exprCall(exprDot(x, identUnix)))))

	return Integer{TypeExpr: identUint32, Size: 4}.EmitMarshaler(secVar, block)
}

func (TimeUnixU32) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	secVar := varFromExpr(x, "sec")
	block = Integer{TypeExpr: identUint32, Size: 4}.EmitUnmarshaler(secVar, block)

	return emitVarAssign(x, exprCall(exprTimeUnix, exprCall(identInt64, secVar), lit0), block)
}

// OptionalPointer wraps a pointer type with a presence byte.
// Marshal: if pointer is nil, writes 0; otherwise writes 1 followed by the value.
// Unmarshal: reads presence byte; if 0, sets nil; if 1, allocates and reads value.
type OptionalPointer struct {
	Type     Type     // inner type (what the pointer points to)
	TypeExpr ast.Expr // the type being pointed to
}

func (o OptionalPointer) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	valBody := o.Type.EmitMarshaler(&ast.ParenExpr{
		X: &ast.StarExpr{X: x},
	}, emitAppendBuf([]ast.Expr{identBuf, lit1}, token.NoPos, nil))

	if valBody == nil {
		return nil
	}

	return append(block, &ast.IfStmt{
		Cond: &ast.BinaryExpr{
			Op: token.EQL,
			X:  x,
			Y:  identNil,
		},
		Body: &ast.BlockStmt{
			List: emitAppendBuf([]ast.Expr{identBuf, lit0}, token.NoPos, nil),
		},
		Else: &ast.BlockStmt{
			List: valBody,
		},
	})
}

func (o OptionalPointer) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	block = emitBoundCheck(exprLenBuf, token.LSS, lit1, false, astToSourceUpper(x), block)

	valBody := emitVarAssign(x, exprCall(identNew, o.TypeExpr), newBlock())
	valBody = o.Type.EmitUnmarshaler(&ast.StarExpr{X: x}, valBody)

	return append(block, &ast.IfStmt{
		Cond: &ast.BinaryExpr{
			Op: token.NEQ,
			X:  exprBuf0,
			Y:  lit0,
		},
		Body: &ast.BlockStmt{List: append([]ast.Stmt{stmtBufRewind(lit1)}, valBody...)},
		Else: &ast.BlockStmt{
			List: append(
				emitVarAssign(x, identNil, nil),
				stmtBufRewind(lit1),
			),
		},
	})
}

// OptionalNull wraps a sql.Null* type with a presence byte.
// Marshal: if .Valid is false, writes 0; otherwise writes 1 followed by the encoded value field.
// Unmarshal: reads presence byte; if 0, sets Valid=false; if 1, sets Valid=true and reads value.
type OptionalNull struct {
	Type       Type       // inner type for the value field
	ValueField *ast.Ident // field name containing the value (e.g. "String", "Int64", "V")
}

func (o OptionalNull) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	valBody := o.Type.EmitMarshaler(
		exprDot(x, o.ValueField),
		emitAppendBuf([]ast.Expr{identBuf, lit1}, token.NoPos, nil),
	)

	if valBody == nil {
		return nil
	}

	return append(block, &ast.IfStmt{
		Cond: exprDot(x, identValid),
		Body: &ast.BlockStmt{List: valBody},
		Else: &ast.BlockStmt{
			List: emitAppendBuf([]ast.Expr{identBuf, lit0}, token.NoPos, nil),
		},
	})
}

func (o OptionalNull) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	block = emitBoundCheck(exprLenBuf, token.LSS, lit1, false, astToSourceUpper(x), block)

	valBody := emitVarAssign(
		fieldSelector(x, identValid),
		identTrue,
		newBlock(),
	)
	valBody = o.Type.EmitUnmarshaler(fieldSelector(x, o.ValueField), valBody)

	return append(block, &ast.IfStmt{
		Cond: &ast.BinaryExpr{
			Op: token.NEQ,
			X:  exprBuf0,
			Y:  lit0,
		},
		Body: &ast.BlockStmt{List: append([]ast.Stmt{stmtBufRewind(lit1)}, valBody...)},
		Else: &ast.BlockStmt{
			List: append(
				emitVarAssign(fieldSelector(x, identValid), identFalse, nil),
				stmtBufRewind(lit1),
			),
		},
	})
}

type Dumb struct{}

func (Dumb) EmitMarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	return append(block, stmtAssign(identBlank, x))
}

func (d Dumb) EmitUnmarshaler(x ast.Expr, block []ast.Stmt) []ast.Stmt {
	return d.EmitMarshaler(x, block)
}
