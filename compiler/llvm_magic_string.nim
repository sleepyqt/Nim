proc build_nim_seq_len(module: BModule; struct: ValueRef): ValueRef =
  # if x != nil: x.sup.length else: 0

  assert_value_type(struct, PointerTypeKind)

  let entry_bb = llvm.getInsertBlock(module.ll_builder)
  let fun = llvm.getBasicBlockParent(entry_bb)
  let then_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "nilcheck.then")
  let else_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "nilcheck.else")
  let cont_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "nilcheck.cont")
  llvm.moveBasicBlockAfter(then_bb, entry_bb)
  llvm.moveBasicBlockAfter(else_bb, then_bb)
  llvm.moveBasicBlockAfter(cont_bb, else_bb)

  # entry:
  llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)
  let null = llvm.constNull(llvm.typeOf(struct))
  let cond = llvm.buildICmp(module.ll_builder, IntNE, struct, null, "nilcheck")
  discard llvm.buildCondBr(module.ll_builder, cond, then_bb, else_bb)

  # nilcheck.then:
  llvm.positionBuilderAtEnd(module.ll_builder, then_bb)
  var indices = [
    constant(module, int32 0), # strip pointer
    constant(module, int32 0), # sup: TGenericSeq field
    constant(module, int32 0)] # length
  let length = llvm.buildLoad(
    module.ll_builder,
    llvm.buildGEP(module.ll_builder, struct, addr indices[0], cuint len indices, ""),
    "")
  discard llvm.buildBr(module.ll_builder, cont_bb)

  # nilcheck.else:
  llvm.positionBuilderAtEnd(module.ll_builder, else_bb)
  let zero = constant_int(module, 0)
  discard llvm.buildBr(module.ll_builder, cont_bb)

  # nilcheck.cont
  llvm.positionBuilderAtEnd(module.ll_builder, cont_bb)
  let phi = llvm.buildPhi(module.ll_builder, module.ll_int, "seq.length")
  var values = [ length, zero ]
  var blocks = [ then_bb, else_bb ]
  llvm.addIncoming(phi, addr values[0], addr blocks[0], 2)

  result = phi

  assert_value_type(result, IntegerTypeKind)

proc gen_magic_append_seq_elem(module: BModule; node: PNode): ValueRef =
  let sq = gen_expr(module, node[1])
  let sq_type = skipTypes(node[1].typ, {tyVar})
  let header = llvm.buildBitCast(module.ll_builder, sq, type_to_ptr get_generic_seq_type(module), "")
  let type_info = gen_type_info(module, sq_type)
  echo "sq ", sq
  echo "sq ty ", llvm.typeOf(sq)
  echo "h ", header
  echo "h ty ", llvm.typeOf(header)
  echo "t ", type_info
  let call = gen_call_runtime_proc(module, "incrSeqV3", @[header, type_info])
  #debug node
  #assert false

proc gen_magic_length_seq(module: BModule; node: PNode): ValueRef =
  let struct = gen_expr(module, node[1])
  result = build_nim_seq_len(module, struct)

proc gen_magic_echo(module: BModule; node: PNode) =
  let brackets = node[1]

  if brackets.len == 0:
    discard
  else:
    let array_ptr = gen_expr(module, brackets) # [1 x %NimStringDesc*]*
    var indices = [constant(module, 0i32), constant(module, 0i32)]
    # get pointer to first element
    let data_ptr = llvm.buildGEP(module.ll_builder, array_ptr, addr indices[0], 2, "") # %NimStringDesc**
    let length = constant_int(module, brackets.len)
    let opn_array = build_open_array(module, brackets.typ.elemType, data_ptr, length, "echo.params")
    discard gen_call_runtime_proc(module, "echoBinSafe", @[opn_array])

proc gen_magic_length_str(module: BModule; node: PNode): ValueRef =
  case node[1].typ.kind:
  of tyCString:
    var args: seq[ValueRef]
    args.add gen_expr(module, node[1])
    result = gen_call_runtime_proc(module, "nimCStrLen", args)
  of tyString:
    let str = gen_expr(module, node[1])
    result = build_nim_seq_len(module, str)
  else:
    assert false

proc gen_magic_append_str_str(module: BModule; node: PNode): ValueRef =
  let str = gen_expr(module, node[1])

  # build `length` expression

  var length = 0
  var length_expr: ValueRef = constant_int(module, 0)
  var strings: seq[ValueRef]
  for i in 2 ..< sonsLen(node):
    strings.add gen_expr(module, node[i])
    if node[i].typ.kind == tyChar:
      inc(length)
    elif node[i].kind in {nkStrLit .. nkTripleStrLit}:
      inc(length, len(node[i].strVal))
    else:
      let string_length = build_nim_seq_len(module, str)
      length_expr = llvm.buildAdd(module.ll_builder, string_length, length_expr, "")

  if length > 0:
    length_expr = llvm.buildAdd(module.ll_builder, constant_int(module, 0), length_expr, "total_length")

  discard gen_call_runtime_proc(module, "resizeString", @[str, length_expr])

  for i in 2 ..< sonsLen(node):
    let str_add = strings[i - 2]
    if node[i].typ.kind == tyChar:
      discard gen_call_runtime_proc(module, "appendChar", @[str, str_add])
    else:
      discard gen_call_runtime_proc(module, "appendString", @[str, str_add])

proc gen_magic_append_str_ch(module: BModule; node: PNode): ValueRef =
  let str = gen_expr(module, node[1])
  let chr = gen_expr(module, node[2])
  result = gen_call_runtime_proc(module, "addChar", @[str, chr])

proc gen_string_to_cstring(module: BModule; node: PNode): ValueRef =
  let str = gen_expr(module, node[0])
  result = gen_call_runtime_proc(module, "nimToCStringConv", @[str])

proc gen_cstring_to_string(module: BModule; node: PNode): ValueRef =
  let cstr = gen_expr(module, node[1])
  result = gen_call_runtime_proc(module, "cstrToNimstr", @[cstr])

proc gen_magic_int_to_str(module: BModule; node: PNode): ValueRef =
  let val = gen_expr(module, node[1])
  result = gen_call_runtime_proc(module, "nimIntToStr", @[val])

proc gen_magic_int64_to_str(module: BModule; node: PNode): ValueRef =
  let val = gen_expr(module, node[1])
  result = gen_call_runtime_proc(module, "nimInt64ToStr", @[val])

proc gen_magic_float_to_str(module: BModule; node: PNode): ValueRef =
  let val = gen_expr(module, node[1])
  result = gen_call_runtime_proc(module, "nimFloatToStr", @[val])

proc gen_magic_char_to_str(module: BModule; node: PNode): ValueRef =
  let val = gen_expr(module, node[1])
  result = gen_call_runtime_proc(module, "nimCharToStr", @[val])

proc gen_magic_bool_to_str(module: BModule; node: PNode): ValueRef =
  let val = gen_expr(module, node[1])
  result = gen_call_runtime_proc(module, "nimBoolToStr", @[val])

proc gen_magic_cstr_to_str(module: BModule; node: PNode): ValueRef =
  let val = gen_expr(module, node[1])
  result = gen_call_runtime_proc(module, "cstrToNimstr", @[val])

proc gen_magic_set_length_str(module: BModule; node: PNode): ValueRef =
  let str = gen_expr(module, node[1])
  let len = gen_expr(module, node[2])
  result = gen_call_runtime_proc(module, "setLengthStr", @[str, len])

proc gen_magic_chr(module: BModule; node: PNode): ValueRef =
  let value = gen_expr(module, node[1])
  let ll_type = get_type(module, node.typ)
  result = llvm.buildTrunc(module.ll_builder, value, ll_type, "magic.chr")

proc gen_magic_con_str_str(module: BModule; node: PNode): ValueRef =
  debug node
  assert false

proc gen_magic_eq_str(module: BModule; node: PNode): ValueRef =
  let lhs = gen_expr(module, node[1])
  let rhs = gen_expr(module, node[2])
  result = gen_call_runtime_proc(module, "eqStrings", @[lhs, rhs])

proc gen_magic_le_str(module: BModule; node: PNode): ValueRef =
  let lhs = gen_expr(module, node[1])
  let rhs = gen_expr(module, node[2])
  let cmp = gen_call_runtime_proc(module, "cmpStrings", @[lhs, rhs])
  let zer = constant_int(module, 0)
  result = llvm.buildICmp(module.ll_builder, IntSLE, cmp, zer, "")
  result = build_i1_to_i8(module, result)

proc gen_magic_lt_str(module: BModule; node: PNode): ValueRef =
  let lhs = gen_expr(module, node[1])
  let rhs = gen_expr(module, node[2])
  let cmp = gen_call_runtime_proc(module, "cmpStrings", @[lhs, rhs])
  let zer = constant_int(module, 0)
  result = llvm.buildICmp(module.ll_builder, IntSLT, cmp, zer, "")
  result = build_i1_to_i8(module, result)
