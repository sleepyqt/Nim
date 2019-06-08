
# Sequences --------------------------------------------------------------------

proc gen_magic_append_seq_elem(module: BModule; node: PNode) =
  # proc incrSeqV3(s: PGenericSeq, typ: PNimType): PGenericSeq {.compilerProc.}

  let seq = gen_expr_lvalue(module, node[1])
  let val = gen_expr(module, node[2])

  assert_value_type(seq.val, PointerTypeKind, PointerTypeKind)

  let seq_val = llvm.buildLoad(module.ll_builder, seq.val, "seq_val")

  let seq_type = skipTypes(node[1].typ, {tyVar})
  let header = build_cast_generic_seq(module, seq_val)
  let type_info = gen_type_info(module, seq_type)
  let call = build_call_runtime(module, "incrSeqV3", @[header, type_info])
  let new_seq = BValue(
    val:     llvm.buildBitCast(module.ll_builder, call, get_type(module, seq_type), "seq"),
    storage: OnHeap)

  # seq = incrSeqV3(seq, ...)
  build_ref_assign(module, seq, new_seq)

  # [1, 2, 3] len 3
  # seq.len = seq.len + 1
  let len_adr = build_nim_seq_len_not_nil_lvalue(module, new_seq.val)
  let len_val = llvm.buildLoad(module.ll_builder, len_adr, "seq.len")
  let new_len = llvm.buildAdd(module.ll_builder, len_val, constant_int(module, 1), "")
  discard llvm.buildStore(module.ll_builder, new_len, len_adr)
  # [1, 2, 3, _] len 4

  # seq[3] = 4
  let last_elem = BValue(
    val:     build_seq_index(module, new_seq.val, len_val),
    storage: seq.storage)
  build_assign(module, last_elem, val, elemType(seq_type), copy = true)

proc gen_magic_new_seq(module: BModule; node: PNode) =
  let seq = gen_expr_lvalue(module, node[1])
  let len = gen_expr(module, node[2])
  build_new_seq(module, seq, node[1].typ, len.val)

proc gen_magic_new_seq_of_cap(module: BModule; node: PNode): BValue =
  # todo
  let cap = gen_expr(module, node[1]).val
  let type_info = gen_type_info(module, node.typ)
  result.val = build_call_runtime(module, "nimNewSeqOfCap", @[type_info, cap])
  result.storage = OnHeap

proc gen_magic_length_seq(module: BModule; node: PNode): ValueRef =
  let struct = gen_expr(module, node[1]).val
  result = build_nim_seq_len(module, struct)

proc gen_magic_set_length_seq(module: BModule; node: PNode) =
  # proc setLengthSeqV2(s: PGenericSeq, typ: PNimType, newLen: int): PGenericSeq
  let seq_node = if node[1].kind in {nkAddr, nkHiddenAddr}: node[1][0] else: node[1]

  let seq = gen_expr_lvalue(module, seq_node)
  let len = gen_expr(module, node[2])

  assert_value_type(seq.val, PointerTypeKind, PointerTypeKind)

  let seq_val = llvm.buildLoad(module.ll_builder, seq.val, "seq_val")

  let seq_type = skipTypes(node[1].typ, {tyVar})
  let header = build_cast_generic_seq(module, seq_val)
  let type_info = gen_type_info(module, skipTypes(seq_type, abstractInst))
  let call = build_call_runtime(module, "setLengthSeqV2", @[header, type_info, len.val])
  let new_seq = BValue(
    val:     llvm.buildBitCast(module.ll_builder, call, get_type(module, seq_type), "seq"),
    storage: OnHeap)

  build_ref_assign(module, seq, new_seq)

# Strings ----------------------------------------------------------------------

proc gen_magic_echo(module: BModule; node: PNode) =
  let brackets = node[1]

  if brackets.len == 0:
    discard
  else:
    let array_ptr = gen_expr(module, brackets).val # [1 x %NimStringDesc*]*
    var indices = [constant(module, 0i32), constant(module, 0i32)]
    # get pointer to first element
    let data_ptr = llvm.buildGEP(module.ll_builder, array_ptr, addr indices[0], 2, "") # %NimStringDesc**
    let length = constant_int(module, brackets.len)
    let opn_array = build_open_array(module, brackets.typ.elemType, data_ptr, length, "echo.params")
    discard build_call_runtime(module, "echoBinSafe", @[opn_array])

proc gen_magic_length_str(module: BModule; node: PNode): ValueRef =
  case node[1].typ.kind:
  of tyCString:
    var args: seq[ValueRef]
    args.add gen_expr(module, node[1]).val
    result = build_call_runtime(module, "nimCStrLen", args)
  of tyString:
    let str = gen_expr(module, node[1]).val
    result = build_nim_seq_len(module, str)
  else:
    assert false

proc gen_magic_append_str_str(module: BModule; node: PNode) =

  # str &= "foo " & "bar" & 'c' * derp
  # resizeString(str, 3 + 3 + 1 + derp.len)
  # appendString(str, "foo")
  # appendString(str, "bar")
  # appendChar(str, 'c')
  # appendString(str, derp)

  let str = gen_expr(module, node[1]).val

  # build `length` expression

  var length = 0
  var length_expr: ValueRef = constant_int(module, 0)
  var strings: seq[ValueRef]
  for i in 2 ..< sonsLen(node):
    let value = gen_expr(module, node[i]).val
    strings.add value
    if node[i].typ.kind == tyChar:
      inc(length)
    elif node[i].kind in {nkStrLit .. nkTripleStrLit}:
      inc(length, len(node[i].strVal))
    else:
      let string_length = build_nim_seq_len(module, value)
      length_expr = llvm.buildAdd(module.ll_builder, string_length, length_expr, "")

  if length > 0:
    length_expr = llvm.buildAdd(module.ll_builder, constant_int(module, 0), length_expr, "total_length")

  discard build_call_runtime(module, "resizeString", @[str, length_expr])

  for i in 2 ..< sonsLen(node):
    let str_add = strings[i - 2]
    if node[i].typ.kind == tyChar:
      discard build_call_runtime(module, "appendChar", @[str, str_add])
    else:
      discard build_call_runtime(module, "appendString", @[str, str_add])

proc gen_magic_con_str_str(module: BModule; node: PNode): BValue =

  # s = "foo " & "bar" & 'c' * derp
  # tmp = rawNewString(3 + 3 + 1 + derp.len)
  # appendString(tmp, "foo")
  # appendString(tmp, "bar")
  # appendChar(tmp, 'c')
  # appendString(tmp, derp)
  # result = s

  # build `length` expression

  var length = 0
  var length_expr: ValueRef = constant_int(module, 0)
  var strings: seq[ValueRef]
  for i in 1 ..< sonsLen(node):
    let value = gen_expr(module, node[i]).val
    strings.add value
    if node[i].typ.kind == tyChar:
      inc(length)
    elif node[i].kind in {nkStrLit .. nkTripleStrLit}:
      inc(length, len(node[i].strVal))
    else:
      let string_length = build_nim_seq_len(module, value)
      length_expr = llvm.buildAdd(module.ll_builder, string_length, length_expr, "")

  if length > 0:
    length_expr = llvm.buildAdd(module.ll_builder, constant_int(module, 0), length_expr, "total_length")

  let tmp = build_call_runtime(module, "rawNewString", @[length_expr])

  for i in 1 ..< sonsLen(node):
    let str_add = strings[i - 1]
    if node[i].typ.kind == tyChar:
      discard build_call_runtime(module, "appendChar", @[tmp, str_add])
    else:
      discard build_call_runtime(module, "appendString", @[tmp, str_add])

  result.val = tmp
  result.storage = OnHeap

proc gen_magic_append_str_ch(module: BModule; node: PNode): ValueRef =
  let str = gen_expr(module, node[1]).val
  let chr = gen_expr(module, node[2]).val
  result = build_call_runtime(module, "addChar", @[str, chr])

proc gen_string_to_cstring(module: BModule; node: PNode): BValue =
  let str = gen_expr(module, node[0]).val
  result.val = build_call_runtime(module, "nimToCStringConv", @[str])

proc gen_cstring_to_string(module: BModule; node: PNode): BValue =
  let cstr = gen_expr(module, node[1]).val
  result.val = build_call_runtime(module, "cstrToNimstr", @[cstr])

proc gen_magic_int_to_str(module: BModule; node: PNode): ValueRef =
  let val = gen_expr(module, node[1]).val
  result = build_call_runtime(module, "nimIntToStr", @[val])

proc gen_magic_int64_to_str(module: BModule; node: PNode): ValueRef =
  let val = gen_expr(module, node[1]).val
  result = build_call_runtime(module, "nimInt64ToStr", @[val])

proc gen_magic_float_to_str(module: BModule; node: PNode): ValueRef =
  let val = gen_expr(module, node[1]).val
  result = build_call_runtime(module, "nimFloatToStr", @[val])

proc gen_magic_char_to_str(module: BModule; node: PNode): ValueRef =
  let val = gen_expr(module, node[1]).val
  result = build_call_runtime(module, "nimCharToStr", @[val])

proc gen_magic_bool_to_str(module: BModule; node: PNode): ValueRef =
  let val = gen_expr(module, node[1]).val
  result = build_call_runtime(module, "nimBoolToStr", @[val])

proc gen_magic_cstr_to_str(module: BModule; node: PNode): ValueRef =
  let val = gen_expr(module, node[1]).val
  result = build_call_runtime(module, "cstrToNimstr", @[val])

proc gen_magic_set_length_str(module: BModule; node: PNode): ValueRef =
  let str = gen_expr(module, node[1]).val
  let len = gen_expr(module, node[2]).val
  result = build_call_runtime(module, "setLengthStr", @[str, len])

proc gen_magic_chr(module: BModule; node: PNode): ValueRef =
  let value = gen_expr(module, node[1]).val
  let ll_type = get_type(module, node.typ)
  result = llvm.buildTrunc(module.ll_builder, value, ll_type, "magic.chr")

proc gen_magic_eq_str(module: BModule; node: PNode): ValueRef =
  let lhs = gen_expr(module, node[1]).val
  let rhs = gen_expr(module, node[2]).val
  result = build_call_runtime(module, "eqStrings", @[lhs, rhs])

proc gen_magic_le_str(module: BModule; node: PNode): ValueRef =
  let lhs = gen_expr(module, node[1]).val
  let rhs = gen_expr(module, node[2]).val
  let cmp = build_call_runtime(module, "cmpStrings", @[lhs, rhs])
  let zer = constant_int(module, 0)
  result = llvm.buildICmp(module.ll_builder, IntSLE, cmp, zer, "")
  result = build_i1_to_i8(module, result)

proc gen_magic_lt_str(module: BModule; node: PNode): ValueRef =
  let lhs = gen_expr(module, node[1]).val
  let rhs = gen_expr(module, node[2]).val
  let cmp = build_call_runtime(module, "cmpStrings", @[lhs, rhs])
  let zer = constant_int(module, 0)
  result = llvm.buildICmp(module.ll_builder, IntSLT, cmp, zer, "")
  result = build_i1_to_i8(module, result)
