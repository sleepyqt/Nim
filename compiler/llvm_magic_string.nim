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
    assert_value_type(str, PointerTypeKind)
    # {{i64, i64}, [i64 x 0]}*
    var indices = [
      constant(module, int32 0), # remove pointer {{i64, i64}, [i64 x 0]}
      constant(module, int32 0), # select first field {i64, i64}
      constant(module, int32 0)  # select lengh field i64
    ]
    let len_field = llvm.buildGEP(module.ll_builder, str, addr indices[0], cuint len indices, "string.length.ptr") # i64*
    # i64*
    result = llvm.buildLoad(module.ll_builder, len_field, "string.length")
    assert_value_type(result, IntegerTypeKind)
  else:
    assert false

proc gen_magic_append_str_str(module: BModule; node: PNode): ValueRef =
  debug node
  assert false

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
