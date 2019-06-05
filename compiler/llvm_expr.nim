# included from "llvm_pass.nim"

proc gen_copy(module: BModule; dst, val: ValueRef; typ: PType) =
  assert dst != nil
  assert val != nil
  assert typ != nil
  assert_value_type(dst, PointerTypeKind)

  case typ.kind:
  of tyObject, tyArray, tyTuple:
    assert_value_type(val, PointerTypeKind)
    let dst_adr = llvm.buildBitCast(module.ll_builder, dst, module.ll_pointer, "")
    let src_adr = llvm.buildBitCast(module.ll_builder, val, module.ll_pointer, "")
    let size = get_type_size(module, typ)
    build_call_memcpy(module, dst_adr, src_adr, size)

  of tyInt .. tyInt64, tyUInt .. tyUInt64:
    assert_value_type(val, IntegerTypeKind)
    discard llvm.buildStore(module.ll_builder, val, dst)

  of tyFloat, tyFloat32, tyFloat64:
    discard llvm.buildStore(module.ll_builder, val, dst)

  of tyPtr, tyPointer, tyCString:
    assert_value_type(val, PointerTypeKind)
    discard llvm.buildStore(module.ll_builder, val, dst)

  of tyBool, tyChar, tyEnum, tyRange:
    assert_value_type(val, IntegerTypeKind)
    discard llvm.buildStore(module.ll_builder, val, dst)

  of tyString:
    assert_value_type(val, PointerTypeKind)
    discard llvm.buildStore(module.ll_builder, val, dst)

  of tyRef:
    assert_value_type(val, PointerTypeKind)
    discard llvm.buildStore(module.ll_builder, val, dst)

  of tySequence:
    assert_value_type(val, PointerTypeKind)
    discard llvm.buildStore(module.ll_builder, val, dst)

  of tySet:
    let size = get_type_size(module, typ)
    case size:
    of 1, 2, 4, 8:
      assert_value_type(val, IntegerTypeKind)
      discard llvm.buildStore(module.ll_builder, val, dst)
    else:
      assert_value_type(val, PointerTypeKind)
      let dst_adr = llvm.buildBitCast(module.ll_builder, dst, module.ll_pointer, "")
      let src_adr = llvm.buildBitCast(module.ll_builder, val, module.ll_pointer, "")
      build_call_memcpy(module, dst_adr, src_adr, size)

  of tyProc:
    assert_value_type(val, PointerTypeKind)
    discard llvm.buildStore(module.ll_builder, val, dst)

  else:
    assert false, "dst: " & $dst & " val: " & $val & " typ: " & $typ.kind

proc gen_copy_lvalue(module: BModule; dst, adr: ValueRef; typ: PType) =
  assert_value_type(dst, PointerTypeKind)
  assert_value_type(adr, PointerTypeKind)
  case typ.kind:
  of tyInt .. tyInt64, tyUint .. tyUint64,
     tyFloat, tyFloat32, tyFloat64,
     tyPtr, tyPointer, tyCString,
     tyBool, tyChar, tyEnum, tyRange:
    let val = llvm.buildLoad(module.ll_builder, adr, "")
    gen_copy(module, dst, val, typ)
  else:
    assert false, $typ.kind

proc gen_ref_copy(module: BModule; dst, adr: ValueRef) =
  assert_value_type(dst, PointerTypeKind)
  assert_value_type(adr, PointerTypeKind)
  discard llvm.buildStore(module.ll_builder, adr, dst)

proc gen_default_init(module: BModule; typ: PType; alloca: ValueRef) =
  assert typ != nil
  assert alloca != nil
  assert_value_type(alloca, PointerTypeKind)
  case typ.kind:
  of tyInt, tyUint:
    discard llvm.buildStore(module.ll_builder, constant_int(module, int8 0), alloca)
  of tyInt8, tyUint8, tyBool, tyChar:
    discard llvm.buildStore(module.ll_builder, constant(module, int8 0), alloca)
  of tyInt16, tyUint16:
    discard llvm.buildStore(module.ll_builder, constant(module, int16 0), alloca)
  of tyInt32, tyUint32:
    discard llvm.buildStore(module.ll_builder, constant(module, int32 0), alloca)
  of tyInt64, tyUInt64:
    discard llvm.buildStore(module.ll_builder, constant(module, int64 0), alloca)
  of tyObject, tyTuple, tyArray:
    let adr = llvm.buildBitCast(module.ll_builder, alloca, module.ll_pointer, "")
    let size = get_type_size(module, typ)
    build_call_memset(module, adr, 0, int64 size)
  of tySet:
    let size = get_type_size(module, typ)
    case size:
    of 1: discard llvm.buildStore(module.ll_builder, constant(module, int8 0), alloca)
    of 2: discard llvm.buildStore(module.ll_builder, constant(module, int16 0), alloca)
    of 4: discard llvm.buildStore(module.ll_builder, constant(module, int32 0), alloca)
    of 8: discard llvm.buildStore(module.ll_builder, constant(module, int64 0), alloca)
    else:
      let adr = llvm.buildBitCast(module.ll_builder, alloca, module.ll_pointer, "")
      build_call_memset(module, adr, 0, int64 size)
  of tyFloat, tyFloat64:
    let zero = llvm.constReal(module.ll_float64, 0d)
    discard llvm.buildStore(module.ll_builder, zero, alloca)
  of tyFloat32:
    let zero = llvm.constReal(module.ll_float32, 0d)
    discard llvm.buildStore(module.ll_builder, zero, alloca)
  of tyPointer, tyPtr, tyRef, tyCString, tyString, tySequence:
    let null = llvm.constNull(get_type(module, typ))
    discard llvm.buildStore(module.ll_builder, null, alloca)
  of tyEnum:
    let zero = llvm.constInt(get_type(module, typ), 0, Bool 0)
    discard llvm.buildStore(module.ll_builder, zero, alloca)
  else: discard

proc convert_scalar(module: BModule; value: ValueRef; dst_type: TypeRef; signed: bool): ValueRef =
  let src_type = llvm.typeOf(value)

  assert_value_type(value, IntegerTypeKind)

  let src_width = llvm.getIntTypeWidth(src_type)
  let dst_width = llvm.getIntTypeWidth(dst_type)

  if src_width > dst_width:
    # truncate
    if signed:
      result = llvm.buildTrunc(module.ll_builder, value, dst_type, "")
    else:
      result = llvm.buildTrunc(module.ll_builder, value, dst_type, "")
  elif src_width < dst_width:
    # extend
    if signed:
      result = llvm.buildSExt(module.ll_builder, value, dst_type, "")
    else:
      result = llvm.buildZExt(module.ll_builder, value, dst_type, "")
  else:
    result = value

proc build_object_init(module: BModule; dest: ValueRef; typ: PType) =
  case analyseObjectWithTypeField(typ):
  of frNone:
    discard
  of frHeader:
    let obj_value = llvm.buildLoad(module.ll_builder, dest, "object")
    let m_type = build_mtype_field_ptr(module, obj_value, typ)
    let type_info = gen_type_info(module, typ)
    discard llvm.buildStore(module.ll_builder, type_info, m_type)
  of frEmbedded:
    let type_info = gen_type_info(module, typ)
    discard gen_call_runtime_proc(module, "objectInit", @[dest, type_info])

proc can_move(module: BModule; val, dst: BValue): bool =
  false # todo

proc build_assign(module: BModule; dst, src: BValue; typ: PType; copy: bool = false) =
  let config = module.module_list.config

  case typ.kind:
  of tyRef:
    if (dst.storage == OnStack and config.selectedGC != gcGo) or usesWriteBarrier(config) == false:
      gen_copy(module, dst.val, src.val, typ)
    elif dst.storage == OnHeap:
      let dst_cast = llvm.buildBitCast(module.ll_builder, dst.val, type_to_ptr module.ll_pointer, "assign.dst")
      let src_cast = llvm.buildBitCast(module.ll_builder, src.val, module.ll_pointer, "assign.src")
      discard gen_call_runtime_proc(module, "asgnRef", @[dst_cast, src_cast])
    else:
      let dst_cast = llvm.buildBitCast(module.ll_builder, dst.val, type_to_ptr module.ll_pointer, "assign.dst")
      let src_cast = llvm.buildBitCast(module.ll_builder, src.val, module.ll_pointer, "assign.src")
      discard gen_call_runtime_proc(module, "unsureAsgnRef", @[dst_cast, src_cast])
  of tyString:
    if config.selectedGC == gcDestructors:
      discard "genGenericAsgn"
    elif (copy == false and src.storage != OnStatic) or can_move(module, src, dst):
      discard "genRefAssign"
    else:
      discard
    gen_copy(module, dst.val, src.val, typ)
  else:
    gen_copy(module, dst.val, src.val, typ)

proc build_new_obj(module: BModule; dest: BValue; typ: PType) =
  assert_value_type(dest.val, PointerTypeKind)

  let config = module.module_list.config

  # todo
  if dest.storage == OnHeap and usesWriteBarrier(config):
    if canFormAcycle(typ):
      discard "nimGCunrefRC1"
    else:
      discard "nimGCunrefNoCycle"
    if config.selectedGC == gcGo:
      discard "newObj"
      discard "unsureAsgnRef"
    else:
      discard "newObj"
      discard "genAssignment"

  let ref_type = skipTypes(typ, abstractInstOwned)
  assert ref_type.kind == tyRef
  let obj_type = lastSon(ref_type)
  let type_info = gen_type_info(module, typ)
  var args: seq[ValueRef]
  args.add type_info
  args.add constant_int(module, int get_type_size(module, typ))
  let allocated_ptr = gen_call_runtime_proc(module, "newObj", args)
  let ref_cast_type = get_type(module, typ)
  let ref_cast = llvm.buildBitCast(module.ll_builder, allocated_ptr, ref_cast_type, "ref_cast")
  gen_ref_copy(module, dest.val, ref_cast)
  build_object_init(module, dest.val, obj_type)
  #todo

proc build_new_seq(module: BModule; dest: ValueRef; typ: PType; length: ValueRef) =
  assert_value_type(dest, PointerTypeKind)
  let type_info = gen_type_info(module, typ)
  var args = @[type_info, length]
  let allocated_seq = gen_call_runtime_proc(module, "newSeq", args) # i8*
  let seq_cast_type = get_type(module, typ)
  assert_value_type(allocated_seq, PointerTypeKind)
  let seq_cast = llvm.buildBitCast(module.ll_builder, allocated_seq, seq_cast_type, "seq_cast")
  gen_ref_copy(module, dest, seq_cast)
  #todo

# - Literals -------------------------------------------------------------------

proc gen_nil_lit(module: BModule; node: PNode): BValue =
  let ll_type = get_type(module, node.typ)
  result.val = llvm.constNull(ll_type)

proc gen_int_lit(module: BModule; node: PNode): BValue =
  let ll_type = get_type(module, node.typ)
  result.val = llvm.constInt(ll_type, culonglong node.intVal, Bool 1)

proc gen_uint_lit(module: BModule; node: PNode): BValue =
  let ll_type = get_type(module, node.typ)
  result.val = llvm.constInt(ll_type, culonglong node.intVal, Bool 0)

proc gen_char_lit(module: BModule; node: PNode): BValue =
  let ll_type = get_type(module, node.typ)
  result.val = llvm.constInt(ll_type, culonglong node.intVal, Bool 0)

proc gen_float_lit(module: BModule; node: PNode): BValue =
  let ll_type = get_type(module, node.typ)
  result.val = llvm.constReal(ll_type, node.floatVal)

proc gen_object_constr(module: BModule; node: PNode): BValue =
  let ll_type = get_type(module, node.typ)
  let alloca = build_entry_alloca(module, ll_type, "object_literal")

  let (dest, obj_type) = if node.typ.kind == tyRef:
    build_new_obj(module, BValue(val: alloca, storage: OnStack), node.typ)
    (llvm.buildLoad(module.ll_builder, alloca, ""), node.typ.lastSon)
  else:
    let adr = llvm.buildBitCast(module.ll_builder, alloca, module.ll_pointer, "")
    let size = get_type_size(module, node.typ)
    build_call_memset(module, adr, 0, size)
    (alloca, node.typ)

  result.val = dest

  for i in 1 ..< node.len:
    let field = node[i]
    let lhs = build_field_access(module, obj_type, dest, field[0].sym)
    let rhs = gen_expr(module, field[1]).val
    gen_copy(module, lhs, rhs, field[0].typ)

proc gen_tuple_constr(module: BModule; node: PNode): BValue =
  let ll_type = get_type(module, node.typ)
  let alloca = build_entry_alloca(module, ll_type, "tuple.constr")
  result.val = alloca
  for i in 0 ..< node.len:
    let field = node[i]
    let index = constant(module, int32(i))
    let lhs = build_field_ptr(module, alloca, index, "tuple.index")
    let rhs = gen_expr(module, field).val
    gen_copy(module, lhs, rhs, field.typ)

proc gen_bracket_lvalue(module: BModule; node: PNode): BValue =
  let ll_type = get_type(module, node.typ)
  result.val = build_entry_alloca(module, ll_type, "bracket")
  case node.typ.kind:
  of tyArray:
    # [i64 x N]*
    for i, item in node.sons:
      let zero = constant_int(module, 0)
      let index = constant_int(module, i)
      var indices = [zero, index]
      let adr = llvm.buildGEP(module.ll_builder, result.val, addr indices[0], cuint len indices, "")
      let val = gen_expr(module, item).val
      gen_copy(module, adr, val, item.typ)
  of tySequence:
    let length = constant_int(module, int node.sonsLen)
    build_new_seq(module, result.val, node.typ, length)
    # {{i64, i64}, i8}**
    let seq = llvm.buildLoad(module.ll_builder, result.val, "")
    # {{i64, i64}, i8}*
    let data_ptr = build_field_ptr(module, seq, constant(module, int32 1))
    # i8*
    assert_value_type(data_ptr, PointerTypeKind)
    for i, item in node.sons:
      var indices = [ constant_int(module, 0), constant_int(module, i) ]
      let adr = llvm.buildGEP(module.ll_builder, data_ptr, addr indices[0], cuint len indices, "")
      let val = gen_expr(module, item).val
      gen_copy(module, adr, val, item.typ)
  else: assert false, $node.typ.kind

proc gen_bracket(module: BModule; node: PNode): BValue =
  result.val = gen_bracket_lvalue(module, node).val
  if not live_as_pointer(module, node.typ):
    result.val = llvm.buildLoad(module.ll_builder, result.val, "")

proc gen_int_set_lit(module: BModule; node: PNode; size: int): BValue =
  let ll_type = get_type(module, node.typ)
  let accum = build_entry_alloca(module, ll_type, "curly.accum")
  gen_default_init(module, node.typ, accum)

  for item in node:
    if item.kind == nkRange:
      discard
    else:
      # accum = accum or (1 shl value)
      let value = gen_expr(module, item).val
      let one = llvm.constInt(ll_type, culonglong 1, Bool 0)
      # todo
      # nim: ../lib/IR/Constants.cpp:1842:
      # static llvm::Constant* llvm::ConstantExpr::get(unsigned int, llvm::Constant*, llvm::Constant*, unsigned int, llvm::Type*):
      # Assertion `C1->getType() == C2->getType()
      # && "Operand types in binary constant expression should match"' failed.
      let bit = llvm.buildShl(module.ll_builder, one, value, "curly.bit")
      let accum_value = llvm.buildLoad(module.ll_builder, accum, "curly.accum.load")
      let merged = llvm.buildOr(module.ll_builder, accum_value, bit, "curly.merged")
      discard llvm.buildStore(module.ll_builder, merged, accum)

  result.val = llvm.buildLoad(module.ll_builder, accum, "curly.load")

proc gen_array_set_lit(module: BModule; node: PNode; size: int): BValue =
  assert false

proc gen_set_lit(module: BModule; node: PNode): BValue =
  #echo "gen_set_lit:"
  let size = int get_type_size(module, node.typ)

  if size <= 8:
    result = gen_int_set_lit(module, node, size)
  else:
    result = gen_array_set_lit(module, node, size)

proc build_cstring_lit(module: BModule; text: string): BValue =
  let ll_type = llvm.arrayType(module.ll_char, cuint len(text) + 1)
  let initializer = llvm.constStringInContext(module.ll_context, text, cuint len text, Bool 0)
  let global = llvm.addGlobal(module.ll_module, ll_type, "literal.cstring")
  llvm.setInitializer(global, initializer)
  llvm.setGlobalConstant(global, Bool 1)
  llvm.setLinkage(global, PrivateLinkage)
  var indices = [constant(module, 0i32), constant(module, 0i32)]
  result.val = llvm.buildGEP(module.ll_builder, global, addr indices[0], cuint len indices, "")

proc gen_str_lit(module: BModule; node: PNode): BValue =
  if node.typ.kind == tyCString:
    result = build_cstring_lit(module, node.strVal)
  elif node.typ.kind == tyString:
    let header_type = get_generic_seq_type(module)
    # todo: flags in cap field
    var header_values = [ constant_int(module, node.strVal.len), constant_int(module, node.strVal.len) ]
    let header = llvm.constStructInContext(module.ll_context, addr header_values[0], 2, Bool 0)

    let str = llvm.constStringInContext(module.ll_context, node.strVal, cuint len node.strVal, Bool 0)
    var nim_str_fields = [ header, str ]

    let initializer = llvm.constStructInContext(module.ll_context, addr nim_str_fields[0], 2, Bool 0)
    let global = llvm.addGlobal(module.ll_module, llvm.typeOf(initializer), "literal.string")
    llvm.setInitializer(global, initializer)
    llvm.setGlobalConstant(global, Bool 1)
    llvm.setLinkage(global, PrivateLinkage)

    result.val = llvm.buildBitCast(module.ll_builder, global, get_nim_string_type(module), "")
  else:
    assert false

# ------------------------------------------------------------------------------

proc fix_literal(module: BModule; node: PNode; lhs, rhs: ValueRef): (ValueRef, ValueRef) =
  # literals sometimes getting wrong types...
  let typ = node[1].typ
  if typ.kind in {tyInt .. tyInt64, tyUint .. tyUint64}:
    let ll_type = get_type(module, typ)
    result[0] = lhs
    result[1] = convert_scalar(module, rhs, ll_type, is_signed_type(typ))
  else:
    result[0] = lhs
    result[1] = rhs

proc gen_logic_or_and(module: BModule; node: PNode; op: TMagic): ValueRef =
  let incoming_bb = llvm.getInsertBlock(module.ll_builder)
  let fun = llvm.getBasicBlockParent(incoming_bb)
  let end_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "or.end")
  var rhs_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "or.rhs")
  llvm.moveBasicBlockAfter(rhs_bb, incoming_bb)
  llvm.moveBasicBlockAfter(end_bb, rhs_bb)

  # evaluate lhs first
  llvm.positionBuilderAtEnd(module.ll_builder, incoming_bb)
  let lhs = build_i8_to_i1(module, gen_expr(module, node[1]).val)
  # reacquire basic block, as new nodes may have inserted
  let lhs_bb = llvm.getInsertBlock(module.ll_builder)
  var expr_result: int
  if op == mOr:
    # short-circuit if lhs is true
    discard llvm.buildCondBr(module.ll_builder, lhs, end_bb, rhs_bb)
    expr_result = 1
  elif op == mAnd:
    # short-circuit if lhs is false
    discard llvm.buildCondBr(module.ll_builder, lhs, rhs_bb, end_bb)
    expr_result = 0

  # evaluate rhs
  llvm.positionBuilderAtEnd(module.ll_builder, rhs_bb)
  let rhs = build_i8_to_i1(module, gen_expr(module, node[2]).val)
  # reacquire rhs basic block
  rhs_bb = llvm.getInsertBlock(module.ll_builder)
  discard llvm.buildBr(module.ll_builder, end_bb)

  # phi node
  llvm.positionBuilderAtEnd(module.ll_builder, end_bb)
  let phi = llvm.buildPHI(module.ll_builder, module.ll_bool, "")
  var values = [llvm.constInt(module.ll_bool, culonglong expr_result, Bool 0), rhs]
  var blocks = [lhs_bb, rhs_bb]
  llvm.addIncoming(phi, addr values[0], addr blocks[0], 2)
  result = build_i1_to_i8(module, phi)

proc gen_magic_inc(module: BModule; node: PNode) =
  let adr = gen_expr_lvalue(module, node[1]).val
  let xincrement = gen_expr(module, node[2]).val
  let xvalue = llvm.buildLoad(module.ll_builder, adr, "")
  let (value, increment) = fix_literal(module, node, xvalue, xincrement)
  let new_value = llvm.buildAdd(module.ll_builder, value, increment, "inc")
  discard llvm.buildStore(module.ll_builder, new_value, adr)

proc gen_magic_dec(module: BModule; node: PNode) =
  let adr = gen_expr_lvalue(module, node[1]).val
  let xincrement = gen_expr(module, node[2]).val
  let xvalue = llvm.buildLoad(module.ll_builder, adr, "")
  let (value, increment) = fix_literal(module, node, xvalue, xincrement)
  let new_value = llvm.buildSub(module.ll_builder, value, increment, "dec")
  discard llvm.buildStore(module.ll_builder, new_value, adr)

proc gen_magic_length(module: BModule; node: PNode): ValueRef =
  #echo "gen_magic_length"
  let sym_node = if node[1].kind == nkHiddenDeref: node[1][0] else: node[1]
  #debug sym_node
  case sym_node.typ.kind:
  of tyOpenArray, tyVarargs:
    let struct = gen_expr_lvalue(module, sym_node).val
    assert_value_type(struct, PointerTypeKind)
    let len_field = build_field_ptr(module, struct, constant(module, 1i32), "openarray.len")
    assert_value_type(len_field, PointerTypeKind)
    result = llvm.buildLoad(module.ll_builder, len_field, "magic.openarray.len")
    assert_value_type(result, IntegerTypeKind)
  else:
    assert false, $sym_node.typ.kind

proc gen_magic_eq_proc(module: BModule; node: PNode): ValueRef =
  if node[1].typ.callConv == ccClosure:
    assert false
  else:
    let lhs = gen_expr(module, node[1]).val
    let rhs = gen_expr(module, node[2]).val
    assert lhs != nil, $node[1].kind
    assert rhs != nil, $node[2].kind
    result = llvm.buildICmp(module.ll_builder, IntEQ, lhs, rhs, "eq.proc")

proc gen_magic_swap(module: BModule; node: PNode): ValueRef =
  let lhs = gen_expr_lvalue(module, node[1]).val
  let rhs = gen_expr_lvalue(module, node[2]).val
  let ll_type = get_type(module, node[1].typ)
  let tmp = build_entry_alloca(module, ll_type, "swap.tmp")
  gen_copy_lvalue(module, tmp, lhs, node[1].typ)
  gen_copy_lvalue(module, lhs, rhs, node[1].typ)
  gen_copy_lvalue(module, rhs, tmp, node[1].typ)

proc gen_magic_new(module: BModule; node: PNode): ValueRef =
  let bval = gen_expr_lvalue(module, node[1])
  result = bval.val
  build_new_obj(module, bval, node[1].typ)

proc gen_magic_is_nil(module: BModule; node: PNode): ValueRef =
  let lhs = gen_expr(module, node[1]).val
  let rhs = llvm.constNull(llvm.typeOf(lhs))
  result = llvm.buildICmp(module.ll_builder, IntEQ, lhs, rhs, "")
  result = build_i1_to_i8(module, result)

proc gen_magic_to_float(module: BModule; node: PNode): ValueRef =
  let value = gen_expr(module, node[1]).val
  result = llvm.buildSIToFP(module.ll_builder, value, get_type(module, node.typ), "m.to.float")

proc gen_magic_ord(module: BModule; node: PNode): ValueRef =
  let value = gen_expr(module, node[1]).val
  let ll_type = get_type(module, node.typ)
  result = convert_scalar(module, value, ll_type, is_signed_type(node[1].typ))

proc gen_magic_of(module: BModule; node: PNode): ValueRef =
  let object_value = gen_expr(module, node[1]).val

  let subclass_type = skipTypes(node[2].typ, typedescPtrs)
  var object_type = skipTypes(node[1].typ, abstractInstOwned)

  assert_value_type(object_value, PointerTypeKind)
  assert subclass_type != nil
  assert object_type != nil

  let m_type_adr = build_mtype_field_ptr(module, object_value, object_type)
  let m_type = llvm.buildLoad(module.ll_builder, m_type_adr, "m_type")
  let object_type_info = gen_type_info(module, object_type)
  let subclass_type_info = gen_type_info(module, subclass_type)

  result = build_nil_check(module, m_type):
    if tfFinal in subclass_type.flags:
      let cmp = llvm.buildICmp(module.ll_builder, IntEQ, m_type, subclass_type_info, "of.typecheck")
      build_i1_to_i8(module, cmp)
    else:
      # todo: cache
      gen_call_runtime_proc(module, "isObj", @[m_type, subclass_type_info])
  do:
    llvm.constInt(module.ll_mem_bool, culonglong 0, Bool 0)

proc gen_magic_expr(module: BModule; node: PNode; op: TMagic): ValueRef =

  proc unary(prc: UnaryProc): ValueRef =
    let lhs = gen_expr(module, node[1]).val
    assert lhs != nil
    result = prc(module.ll_builder, lhs, "")

  proc binary(prc: BinaryProc): ValueRef =
    let xlhs = gen_expr(module, node[1]).val
    let xrhs = gen_expr(module, node[2]).val
    let (lhs, rhs) = fix_literal(module, node, xlhs, xrhs)
    result = prc(module.ll_builder, lhs, rhs, "")

  proc cmp_int(pred: IntPredicate): ValueRef =
    let xlhs = gen_expr(module, node[1]).val
    let xrhs = gen_expr(module, node[2]).val
    let (lhs, rhs) = fix_literal(module, node, xlhs, xrhs)
    result = llvm.buildICmp(module.ll_builder, pred, lhs, rhs, "")
    result = build_i1_to_i8(module, result)

  proc unary_minus(): ValueRef =
    # -value
    let value = gen_expr(module, node[1]).val
    result = llvm.buildNeg(module.ll_builder, value, "unary_minus")

  proc abs_int(): ValueRef =
    # if x >= 0 x else: -x
    let pred = if is_signed_type(node[1].typ): llvm.IntSGE else: llvm.IntUGe
    let value = gen_expr(module, node[1]).val
    let zero = llvm.constInt(llvm.typeOf(value), 0, Bool 1)
    let neg_value = llvm.buildNeg(module.ll_builder, value, "abs.neg_value")
    let cond = llvm.buildICmp(module.ll_builder, pred, value, zero, "abs.cmp")
    result = llvm.buildSelect(module.ll_builder, cond, value, neg_value, "abs.result")

  proc min_int(): ValueRef =
    # if x <= y: x else: y
    let pred = if is_signed_type(node[1].typ): llvm.IntSLE else: llvm.IntULE
    let x = gen_expr(module, node[1]).val
    let y = gen_expr(module, node[2]).val
    let cond = llvm.buildICmp(module.ll_builder, pred, x, y, "min.cond")
    result = llvm.buildSelect(module.ll_builder, cond, x, y, "min.result")

  proc max_int(): ValueRef =
    # if x >= y: x else: y
    let pred = if is_signed_type(node[1].typ): llvm.IntSGE else: llvm.IntUGE
    let x = gen_expr(module, node[1]).val
    let y = gen_expr(module, node[2]).val
    let cond = llvm.buildICmp(module.ll_builder, pred, x, y, "max.cond")
    result = llvm.buildSelect(module.ll_builder, cond, x, y, "max.result")

  proc float_binary(prc: BinaryProc): ValueRef =
    let lhs = gen_expr(module, node[1]).val
    let rhs = gen_expr(module, node[2]).val
    assert lhs != nil
    assert rhs != nil
    result = prc(module.ll_builder, lhs, rhs, "")

  proc abs_float(): ValueRef =
    # if x >= 0 x else: -x
    let value = gen_expr(module, node[1]).val
    let zero = llvm.constReal(llvm.typeOf(value), 0)
    let neg_value = llvm.buildFNeg(module.ll_builder, value, "abs.neg_value")
    let cond = llvm.buildFCmp(module.ll_builder, RealOGE, value, zero, "abs.cmp")
    result = llvm.buildSelect(module.ll_builder, cond, value, neg_value, "abs.result")

  proc min_float(): ValueRef =
    # if x <= y: x else: y
    let x = gen_expr(module, node[1]).val
    let y = gen_expr(module, node[2]).val
    let cond = llvm.buildFCmp(module.ll_builder, llvm.RealOLE, x, y, "")
    result = llvm.buildSelect(module.ll_builder, cond, x, y, "")

  proc max_float(): ValueRef =
    # if x >= y: x else: y
    let x = gen_expr(module, node[1]).val
    let y = gen_expr(module, node[2]).val
    let cond = llvm.buildFCmp(module.ll_builder, llvm.RealOGE, x, y, "")
    result = llvm.buildSelect(module.ll_builder, cond, x, y, "")

  proc cmp_float(pred: RealPredicate): ValueRef =
    let lhs = gen_expr(module, node[1]).val
    let rhs = gen_expr(module, node[2]).val
    result = llvm.buildFCmp(module.ll_builder, pred, lhs, rhs, "")
    result = build_i1_to_i8(module, result)

  case op:
  of mMinI: result = min_int()
  of mMaxI: result = max_int()
  of mAbsI: result = abs_int()
  # signed int
  of mAddI: result = binary(llvm.buildAdd)
  of mSubI: result = binary(llvm.buildSub)
  of mMulI: result = binary(llvm.buildMul)
  of mDivI: result = binary(llvm.buildSDiv)
  of mModI: result = binary(llvm.buildSRem)
  of mUnaryMinusI, mUnaryMinusI64: result = unary_minus()
  of mUnaryPlusI: result = gen_expr(module, node[1]).val
  # unsigned int
  of mAddU: result = binary(llvm.buildAdd)
  of mSubU: result = binary(llvm.buildSub)
  of mMulU: result = binary(llvm.buildMul)
  of mDivU: result = binary(llvm.buildUDiv)
  of mModU: result = binary(llvm.buildURem)
  # integer cmp
  of mEqI: result = cmp_int(llvm.IntEQ)
  of mLeI: result = cmp_int(llvm.IntSLE)
  of mLtI: result = cmp_int(llvm.IntSLT)
  # unsigned cmp
  of mLeU: result = cmp_int(llvm.IntULE)
  of mLtU: result = cmp_int(llvm.IntULT)
  # char cmp
  of mEqCh: result = cmp_int(llvm.IntEQ)
  of mLtCh: result = cmp_int(llvm.IntULT)
  of mLeCh: result = cmp_int(llvm.IntULE)
  # bool cmp
  of mEqB: result = cmp_int(llvm.IntEQ)
  of mLtB: result = cmp_int(llvm.IntULT)
  of mLeB: result = cmp_int(llvm.IntULE)
  # ref cmp
  of mEqRef: result = cmp_int(llvm.IntEQ)
  of mEqUntracedRef: result = cmp_int(llvm.IntEQ)
  of mLePtr: result = cmp_int(llvm.IntULE)
  of mLtPtr: result = cmp_int(llvm.IntULT)
  # enum cmp
  of mEqEnum: result = cmp_int(llvm.IntEQ)
  of mLeEnum: result = cmp_int(if is_signed_type(node.typ): IntSLE else: IntULE)
  of mLtEnum: result = cmp_int(if is_signed_type(node.typ): IntSLT else: IntULT)
  # proc cmp
  of mEqProc: result = gen_magic_eq_proc(module, node)
  #of mLeU64: discard
  #of mLtU64: discard
  # boolean
  of mOr, mAnd: result = gen_logic_or_and(module, node, op)
  of mXor: result = binary(llvm.buildXor)
  of mNot: result = unary(llvm.buildNot)
  # bitwise
  of mShrI: result = binary(llvm.buildLShr)
  of mShlI: result = binary(llvm.buildShl)
  of mAshrI: result = binary(llvm.buildAShr)
  of mBitandI: result = binary(llvm.buildAnd)
  of mBitorI: result = binary(llvm.buildOr)
  of mBitxorI: result = binary(llvm.buildXor)
  of mBitnotI: result = unary(llvm.buildNot)
  # float
  of mMinF64: result = min_float()
  of mMaxF64: result = max_float()
  of mAbsF64: result = abs_float()
  # float arith
  of mAddF64: result = float_binary(llvm.buildFAdd)
  of mSubF64: result = float_binary(llvm.buildFSub)
  of mMulF64: result = float_binary(llvm.buildFMul)
  of mDivF64: result = float_binary(llvm.buildFDiv)
  # float cmp
  of mEqF64: result = cmp_float(llvm.RealOEQ)
  of mLeF64: result = cmp_float(llvm.RealOLE)
  of mLtF64: result = cmp_float(llvm.RealOLT)
  # misc
  of mEcho: gen_magic_echo(module, node)
  of mInc: gen_magic_inc(module, node)
  of mDec: gen_magic_dec(module, node)
  of mLengthOpenArray: result = gen_magic_length(module, node)
  of mSwap: result = gen_magic_swap(module, node)
  of mOrd: result = gen_magic_ord(module, node)
  of mDotDot: result = gen_call_runtime_proc(module, node)
  of mOf: result = gen_magic_of(module, node)
  # sets
  of mIncl: gen_incl_set(module, node)
  of mExcl: gen_excl_set(module, node)
  of mCard: result = gen_card_set(module, node)
  of mLtSet: result = gen_lt_set(module, node)
  of mLeSet: result = gen_le_set(module, node)
  of mEqSet: result = gen_eq_set(module, node)
  of mMulSet: result = gen_mul_set(module, node)
  of mPlusSet: result = gen_plus_set(module, node)
  of mMinusSet: result = gen_minus_set(module, node)
  of mSymDiffSet: result = gen_sym_diff_set(module, node)
  of mInSet: result = gen_magic_in_set(module, node)
  # Heap
  of mNew: result = gen_magic_new(module, node)
  of mIsNil: result = gen_magic_is_nil(module, node)
  # seq
  of mLengthSeq: result = gen_magic_length_seq(module, node)
  of mAppendSeqElem: result = gen_magic_append_seq_elem(module, node)
  of mNewSeq: result = gen_magic_new_seq(module, node)
  of mNewSeqOfCap: result = gen_magic_new_seq_of_cap(module, node)
  # strings
  of mLengthStr: result = gen_magic_length_str(module, node)
  of mNewString, mNewStringOfCap, mExit, mParseBiggestFloat:
    result = gen_call_runtime_proc(module, node)
  of mCopyStr: result = gen_call_runtime_proc(module, node)
  of mEqCString: result = gen_call_expr(module, node)
  # of mSetLengthStr:
  # of mCopyStr:
  # of mCopyStrLast:
  of mConStrStr: result = gen_magic_con_str_str(module, node)
  of mAppendStrCh: result = gen_magic_append_str_ch(module, node)
  of mAppendStrStr: result = gen_magic_append_str_str(module, node)
  of mSetLengthStr: result = gen_magic_set_length_str(module, node)
  of mChr: result = gen_magic_chr(module, node)
  # of mAppendSeqElem:
  of mEqStr: result = gen_magic_eq_str(module, node)
  of mLeStr: result = gen_magic_le_str(module, node)
  of mLtStr: result = gen_magic_lt_str(module, node)
  of mIntToStr: result = gen_magic_int_to_str(module, node)
  of mInt64ToStr: result = gen_magic_int64_to_str(module, node)
  of mBoolToStr: result = gen_magic_bool_to_str(module, node)
  of mCharToStr: result = gen_magic_char_to_str(module, node)
  of mFloatToStr: result = gen_magic_float_to_str(module, node)
  of mCStrToStr: result = gen_magic_cstr_to_str(module, node)
  # of mStrToStr:
  # of mEnumToStr:
  # conv
  of mToFloat: result = gen_magic_to_float(module, node)
  of mZe8ToI .. mZeIToI64:
    let val = gen_expr(module, node[1]).val
    result = llvm.buildZExt(module.ll_builder, val, get_type(module, node.typ), "zetoi")
  of mToU8 .. mToU32:
    let val = gen_expr(module, node[1]).val
    result = llvm.buildTrunc(module.ll_builder, val, get_type(module, node.typ), "tou")
  else: echo "unknown magic: ", op, " ", file_info(module, node)

proc gen_obj_down_conv(module: BModule; node: PNode): BValue =
  let ll_type = get_type(module, node.typ)
  let value = gen_expr(module, node[0])
  let conv_type = if node.typ.kind == tyObject: type_to_ptr ll_type else: ll_type
  assert llvm.getTypeKind(conv_type) == PointerTypeKind
  assert_value_type(value.val, PointerTypeKind)
  result.val = llvm.buildBitCast(module.ll_builder, value.val, conv_type, "obj.down")
  result.storage = value.storage

proc gen_obj_up_conv(module: BModule; node: PNode): BValue =
  let ll_type = get_type(module, node.typ)
  let value = gen_expr(module, node[0])
  assert llvm.getTypeKind(ll_type) == PointerTypeKind
  assert_value_type(value.val, PointerTypeKind)
  result.val = llvm.buildBitCast(module.ll_builder, value.val, ll_type, "obj.up")
  result.storage = value.storage

# ------------------------------------------------------------------------------

proc gen_call(module: BModule; node: PNode): BValue =
  if node[0].kind == nkSym and node[0].sym.magic != mNone:
    result.val = gen_magic_expr(module, node, node[0].sym.magic)
  else:
    result.val = gen_call_expr(module, node)

# ------------------------------------------------------------------------------

proc gen_sym_const(module: BModule; sym: PSym): BValue =
  result.val = module.get_value(sym)
  if result.val == nil:
    case sym.typ.kind:
    of tyArray:
      let elem_type = get_type(module, elemType(sym.typ))
      let elem_count = cuint lengthOrd(module.module_list.config, sym.typ)
      var items: seq[ValueRef]
      for item in sym.ast:
        items.add gen_expr(module, item).val
      let data = llvm.constArray(elem_type, addr items[0], cuint elem_count)
      result.val = llvm.addGlobal(module.ll_module, llvm.typeOf(data), "constant")
      llvm.setGlobalConstant(result.val, Bool 1)
      llvm.setInitializer(result.val, data)
      module.add_value(sym, result.val)
      result.storage = OnHeap
    of tyInt: # produced by {.intdefine.}
      let ll_type = get_type(module, sym.typ)
      result.val = constInt(ll_type, culonglong sym.ast.intVal, Bool is_signed_type(sym.typ))
      result.storage = OnStack
    else:
      assert false, $sym.typ.kind

  assert result.val != nil, (block: (debug(sym); "nil"))

proc gen_sym_expr_lvalue(module: BModule; node: PNode): BValue =
  #echo "gen_sym_expr_lvalue kind: ", node.sym.kind

  case node.sym.kind:
  of skVar, skForVar, skLet, skResult, skTemp:
    if {sfGlobal, sfThread} * node.sym.flags != {}:
      result = gen_var_prototype(module, node)
    else:
      result.val = module.get_value(node.sym)
      result.storage = OnStack
  of skParam:
    result.val = module.get_value(node.sym)
    result.storage = OnStack
  of skConst:
    result.val = gen_sym_const(module, node.sym).val
  else:
    assert false, $node.sym.kind & file_info(module, node)

  assert result.val != nil
  assert_value_type(result.val, PointerTypeKind)

proc gen_sym_expr(module: BModule; node: PNode): BValue =
  #echo "gen_sym_expr kind: ", node.kind, " sym.kind: ", node.sym.kind

  case node.sym.kind:
  of skProc, skConverter, skIterator, skFunc:
    if sfCompileTime in node.sym.flags: module.ice("attempt to generate code for compile time proc")
    result.val = gen_proc(module, node.sym)
  of skConst:
    result = gen_sym_const(module, node.sym)
  of skVar, skForVar, skResult, skLet, skParam, skTemp:
    let alloca = gen_sym_expr_lvalue(module, node)
    if live_as_pointer(module, node.sym.typ):
      result = alloca
    else:
      result.val = llvm.buildLoad(module.ll_builder, alloca.val, "var." & node.sym.name.s)
      result.storage = alloca.storage
  of skType:
    result.val = gen_type_info(module, node.sym.typ)
  else:
    assert false, $node.sym.kind & file_info(module, node)

  assert result.val != nil

# ------------------------------------------------------------------------------

proc gen_dot_expr_lvalue(module: BModule; node: PNode): BValue =
  let lhs = gen_expr_lvalue(module, node[0])

  let field = node[1].sym
  let object_type = node[0].typ

  if node[0].typ.kind == tyObject:
    result.val = build_field_access(module, object_type, lhs.val, field)
    result.storage = lhs.storage
  elif node[0].typ.kind == tyTuple:
    for index, tuple_field in node[0].typ.n.sons:
      if tuple_field.kind == nkSym:
        if tuple_field.sym.id == field.id:
          let ll_index = constant(module, int32 index)
          result.val = build_field_ptr(module, lhs.val, ll_index, "tuple.field")
          result.storage = lhs.storage
          break
  else:
    assert false, $(node[0].typ.kind)

  assert result.val != nil

proc gen_dot_expr(module: BModule; node: PNode): BValue =
  let adr = gen_dot_expr_lvalue(module, node)
  assert_value_type(adr.val, PointerTypeKind)

  if live_as_pointer(module, node[1].typ):
    result = adr
  else:
    result.val = llvm.buildLoad(module.ll_builder, adr.val, "gen_dot_expr.deref")
    result.storage = adr.storage

  assert result.val != nil

proc gen_checked_field_lvalue(module: BModule; node: PNode): BValue =
  # todo: field checks
  result = gen_dot_expr_lvalue(module, node[0])

  #assert result != nil

proc gen_checked_field_expr(module: BModule; node: PNode): BValue =
  let adr = gen_checked_field_lvalue(module, node).val
  result.val = llvm.buildLoad(module.ll_builder, adr, "gen_checked_field_expr.deref")

  assert result.val != nil

# ------------------------------------------------------------------------------

proc gen_bracket_expr_lvalue(module: BModule; node: PNode): BValue =
  let lhs = gen_expr_lvalue(module, node[0])
  let rhs = gen_expr(module, node[1])
  assert lhs.val != nil
  assert rhs.val != nil
  case node[0].typ.kind:
  of tyArray:
    var indices = [constant_int(module, 0), rhs.val]
    result.val = llvm.buildGEP(module.ll_builder, lhs.val, addr indices[0], cuint len indices, "array.index")
    result.storage = lhs.storage
  of tyUncheckedArray:
    var indices = [constant_int(module, 0), rhs.val]
    result.val = llvm.buildGEP(module.ll_builder, lhs.val, addr indices[0], cuint len indices, "uncheckedarray.index")
  of tyOpenArray, tyVarargs:
    let adr_field_data = build_field_ptr(module, lhs.val, constant(module, 0i32), "openarray.data")
    let adr_field_lengt = build_field_ptr(module, lhs.val, constant(module, 1i32), "openarray.length")
    let data_ptr = llvm.buildLoad(module.ll_builder, adr_field_data, "openarray.deref")
    var indices = [rhs.val]
    result.val = llvm.buildGEP(module.ll_builder, data_ptr, addr indices[0], cuint len indices, "openarray.index")
    result.storage = lhs.storage
  of tyCString:
    let adr = llvm.buildLoad(module.ll_builder, lhs.val, "cstring.deref")
    var indices = [rhs.val]
    result.val = llvm.buildGEP(module.ll_builder, adr, addr indices[0], cuint len indices, "cstring.index")
    result.storage = lhs.storage
  of tyString:
    # {{i64, i64}, [i8 x 0]}**
    let struct = llvm.buildLoad(module.ll_builder, lhs.val, "")
    # {{i64, i64}, [i8 x 0]}*
    let data = build_field_ptr(module, struct, constant(module, int32 1), "string.data.ptr")
    # [i8 x 0]*
    var indices = [constant_int(module, 0), rhs.val]
    result.val = llvm.buildGEP(module.ll_builder, data, addr indices[0], cuint len indices, "string.[]")
    result.storage = lhs.storage
    # i8*
  of tySequence:
    let struct = llvm.buildLoad(module.ll_builder, lhs.val, "")
    let data = build_field_ptr(module, struct, constant(module, int32 1), "seq.data.ptr")
    var indices = [constant_int(module, 0), rhs.val]
    result.val = llvm.buildGEP(module.ll_builder, data, addr indices[0], cuint len indices, "seq.[]")
    result.storage = lhs.storage
  of tyTuple:
    # {i64, i64, i64}**
    let struct = llvm.buildLoad(module.ll_builder, lhs.val, "")
    # {i64, i64, i64}*
    result.val = build_field_ptr(module, struct, rhs.val, "tuple.[]")
    result.storage = lhs.storage
  else:
    assert false

proc gen_bracket_expr(module: BModule; node: PNode): BValue =
  let adr = gen_bracket_expr_lvalue(module, node)
  result.val = llvm.buildLoad(module.ll_builder, adr.val, "gen_bracket_expr.deref")
  result.storage = adr.storage

# ------------------------------------------------------------------------------

proc gen_deref_expr_lvalue(module: BModule; node: PNode): BValue =
  # **value -> *value
  let adr = gen_expr_lvalue(module, node[0])
  # usrToCell(p).refcount
  # usrToCell(p)[].refcount ignore
  if node[0].kind == nkCall and live_as_pointer(module, node.typ) and node.kind == nkHiddenDeref:
    result = adr
  else:
    result.val = llvm.buildLoad(module.ll_builder, adr.val, "deref.lvalue")

proc gen_deref_expr(module: BModule; node: PNode): BValue =
  # *value -> value
  let adr = gen_expr(module, node[0]).val
  if node[0].typ.kind == tyVar and live_as_pointer(module, node.typ) and node.kind == nkHiddenDeref:
    # proc p1(p: Foo)
    # proc p2(p: var Foo) = p1(p)
    #                          p[] # ← ignore hidden deref here
    result.val = adr
  else:
    result.val = llvm.buildLoad(module.ll_builder, adr, "deref.rvalue")

proc gen_addr(module: BModule; node: PNode): BValue =
  # addr foo
  result.val = gen_expr_lvalue(module, node[0]).val

proc gen_conv(module: BModule; node: PNode): BValue =
  let dst_type = skipTypes(node.typ, {tyRange})
  let src_type = skipTypes(node[1].typ, {tyRange})
  let value = gen_expr(module, node[1]).val
  let ll_src_type = llvm.typeOf(value)
  let ll_dst_type = get_type(module, dst_type)

  const Integers   = {tyInt .. tyInt64}
  const Floats     = {tyFloat .. tyFloat128}
  const Unsigned   = {tyUint .. tyUInt64, tyChar, tyEnum}
  const Pointers   = {tyPtr, tyPointer, tyCString, tyRef}

  if ll_src_type == ll_dst_type:
    result.val = value

  elif (src_type.kind in Integers) and (dst_type.kind in Integers):
    # int -> int
    result.val = convert_scalar(module, value, ll_dst_type, true)

  elif (src_type.kind in Unsigned) and (dst_type.kind in Unsigned):
    # uint -> uint
    result.val = convert_scalar(module, value, ll_dst_type, false)

  elif (src_type.kind in {tyFloat32}) and (dst_type.kind in {tyFloat, tyFloat64}):
    # ext float
    result.val = llvm.buildFPExt(module.ll_builder, value, module.ll_float64, "")

  elif (src_type.kind in {tyFloat64, tyFloat}) and (dst_type.kind in {tyFloat32}):
    # trunc float
    result.val = llvm.buildFPTrunc(module.ll_builder, value, module.ll_float32, "")

  elif (src_type.kind in Floats) and (dst_type.kind in Integers):
    # float -> int
    result.val = llvm.buildFPToSI(module.ll_builder, value, ll_dst_type, "")

  elif (src_type.kind in Floats) and (dst_type.kind in Unsigned):
    # float -> uint
    result.val = llvm.buildFPToUI(module.ll_builder, value, ll_dst_type, "")

  elif (src_type.kind in Integers) and (dst_type.kind in Floats):
    # int -> float
    result.val = llvm.buildSIToFP(module.ll_builder, value, ll_dst_type, "")

  elif (src_type.kind in Unsigned) and (dst_type.kind in Floats):
    # uint -> float
    result.val = llvm.buildUIToFP(module.ll_builder, value, ll_dst_type, "")

  elif (src_type.kind in Pointers) and (dst_type.kind in Pointers):
    # pointer -> pointer
    result.val = llvm.buildBitCast(module.ll_builder, value, ll_dst_type, "")

  elif (src_type.kind == tyEnum) and (dst_type.kind in Integers):
    # enum -> integer
    result.val = convert_scalar(module, value, ll_dst_type, false)

  else:
    echo "unsupported conv ", src_type.kind, " =====> ", dst_type.kind

proc gen_check_range_float(module: BModule; node: PNode): BValue =
  # todo
  discard

proc gen_check_range(module: BModule; node: PNode): BValue =
  # todo: range check
  let value = gen_expr(module, node[0]).val
  let dst_type = get_type(module, node.typ)
  let signed = is_signed_type(node[0].typ)
  result.val = convert_scalar(module, value, dst_type, signed)

proc gen_check_range64(module: BModule; node: PNode): BValue =
  # todo
  let value = gen_expr(module, node[0]).val
  let dst_type = get_type(module, node.typ)
  let signed = is_signed_type(node[0].typ)
  result.val = convert_scalar(module, value, dst_type, signed)

proc build_cast(module: BModule; value: ValueRef; ll_dst_type: TypeRef): ValueRef =
  let ll_src_type = llvm.typeOf(value)
  let src_kind = llvm.getTypeKind(ll_src_type)
  let dst_kind = llvm.getTypeKind(ll_dst_type)
  if   src_kind == PointerTypeKind and dst_kind == IntegerTypeKind:
    result = llvm.buildPtrToInt(module.ll_builder, value, ll_dst_type, "cast.ptr_to_int")
  elif src_kind == IntegerTypeKind and dst_kind == PointerTypeKind:
    result = llvm.buildIntToPtr(module.ll_builder, value, ll_dst_type, "cast.int_to_ptr")
  elif src_kind == PointerTypeKind and dst_kind == PointerTypeKind:
    result = llvm.buildBitCast(module.ll_builder, value, ll_dst_type, "cast.ptr_to_ptr")
  elif ll_src_type == ll_dst_type:
    result = value
  else:
    echo "build_cast:"
    echo "src ty < ", ll_src_type, " >"
    echo "dst ty < ", ll_dst_type, " >"
    echo "value < ", value, " >"
    echo "src_kind < ", src_kind, " >"
    echo "dst_kind < ", dst_kind, " >"
    assert false

proc gen_cast_expr_lvalue(module: BModule; node: PNode): BValue =
  let value = gen_expr_lvalue(module, node[1]).val
  let dst_type = type_to_ptr get_type(module, node[0].typ)
  result.val = build_cast(module, value, dst_type)

proc gen_cast_expr(module: BModule; node: PNode): BValue =
  let value = gen_expr(module, node[1]).val
  let dst_type = get_type(module, node[0].typ)
  result.val = build_cast(module, value, dst_type)

# ------------------------------------------------------------------------------

proc gen_expr_lvalue(module: BModule; node: PNode): BValue =
  # L value, always pointer
  #echo "gen_expr_lvalue kind: ", node.kind, " ", file_info(module, node)
  case node.kind:
  of nkSym:
    result = gen_sym_expr_lvalue(module, node)
    assert_value_type(result.val, PointerTypeKind)
  of nkDotExpr:
    result = gen_dot_expr_lvalue(module, node)
    assert_value_type(result.val, PointerTypeKind)
  of nkCheckedFieldExpr:
    result = gen_checked_field_lvalue(module, node)
    assert_value_type(result.val, PointerTypeKind)
  of nkBracketExpr:
    result = gen_bracket_expr_lvalue(module, node)
    assert_value_type(result.val, PointerTypeKind)
  of nkHiddenDeref, nkDerefExpr:
    result = gen_deref_expr_lvalue(module, node)
    assert_value_type(result.val, PointerTypeKind)
  of nkCast:
    result = gen_cast_expr_lvalue(module, node)
    assert_value_type(result.val, PointerTypeKind)
  of nkBracket:
    result = gen_bracket_lvalue(module, node)
    assert_value_type(result.val, PointerTypeKind)
  of nkCall:
    result = gen_call(module, node)
    assert_value_type(result.val, PointerTypeKind)
  else: echo "gen_expr_lvalue: unknown node kind: ", node.kind, " ", file_info(module, node)

  if llvm.getTypeKind(llvm.typeOf(result.val)) != PointerTypeKind:
    echo "! ", file_info(module, node)
    echo llvm.getBasicBlockParent(llvm.getInsertBlock(module.ll_builder))
    echo "node"
    debug node

proc gen_expr(module: BModule; node: PNode): BValue =
  # R value, can be pointer or value
  #echo "gen_expr kind: ", node.kind, " ", file_info(module, node)
  case node.kind:
  of nkSym:
    result = gen_sym_expr(module, node)
  of nkNilLit:
    result = gen_nil_lit(module, node)
  of nkStrLit .. nkTripleStrLit:
    result = gen_str_lit(module, node)
  of nkIntLit .. nkInt64Lit:
    result = gen_int_lit(module, node)
  of nkUintLit .. nkUInt64Lit:
    result = gen_uint_lit(module, node)
  of nkFloatLit..nkFloat64Lit:
    result = gen_float_lit(module, node)
  of nkCharLit:
    result = gen_char_lit(module, node)
  of nkCall, nkHiddenCallConv, nkInfix, nkPrefix, nkPostfix, nkCommand, nkCallStrLit:
    result = gen_call(module, node)
  of nkCurly:
    result = gen_set_lit(module, node)
  of nkBracket:
    result = gen_bracket(module, node)
  of nkPar, nkTupleConstr:
    result = gen_tuple_constr(module, node)
  of nkObjConstr:
    result = gen_object_constr(module, node)
  of nkCast:
    result = gen_cast_expr(module, node)
  of nkHiddenStdConv, nkHiddenSubConv, nkConv:
    result = gen_conv(module, node)
  of nkHiddenAddr, nkAddr:
    result = gen_addr(module, node)
  of nkBracketExpr:
    result = gen_bracket_expr(module, node)
  of nkDerefExpr, nkHiddenDeref:
    result = gen_deref_expr(module, node)
  of nkDotExpr:
    result = gen_dot_expr(module, node)
  of nkCheckedFieldExpr:
    result = gen_checked_field_expr(module, node)
  of nkBlockExpr, nkBlockStmt:
    result = gen_block(module, node)
  of nkStmtListExpr:
    result = gen_stmt_list_expr(module, node)
  of nkStmtList:
    gen_stmt_list(module, node)
  of nkIfExpr, nkIfStmt:
    result = gen_if(module, node)
  of nkWhen:
    result = gen_expr(module, node[1][0])
  of nkObjDownConv:
    result = gen_obj_down_conv(module, node)
  of nkObjUpConv:
    result = gen_obj_up_conv(module, node)
  of nkChckRangeF:
    result = gen_check_range_float(module, node)
  of nkChckRange64:
    result = gen_check_range64(module, node)
  of nkChckRange:
    result = gen_check_range(module, node)
  of nkLambdaKinds:
    discard
  of nkClosure:
    discard
  of nkEmpty:
    discard
  of nkWhileStmt:
    gen_while(module, node)
  of nkVarSection, nkLetSection:
    gen_var_section(module, node)
  of nkConstSection:
    discard
  of nkForStmt:
    discard
  of nkCaseStmt:
    gen_case_stmt(module, node)
  of nkReturnStmt:
    gen_return(module, node)
  of nkBreakStmt:
    gen_break(module, node)
  of nkAsgn, nkFastAsgn:
    gen_asgn(module, node)
  of nkDiscardStmt:
    gen_discard(module, node)
  of nkAsmStmt:
    discard
  of nkTryStmt, nkHiddenTryStmt:
    gen_try(module, node)
  of nkRaiseStmt:
    gen_raise(module, node)
  of nkTypeSection:
    discard
  of nkCommentStmt, nkIteratorDef, nkIncludeStmt, nkImportStmt, nkImportExceptStmt, nkExportStmt, nkExportExceptStmt, nkFromStmt, nkTemplateDef, nkMacroDef, nkStaticStmt:
    discard
  of nkPragma:
    discard
  of nkPragmaBlock:
    result = gen_expr(module, node.lastSon)
  of nkProcDef, nkFuncDef, nkMethodDef, nkConverterDef:
    if node.sons[genericParamsPos].kind == nkEmpty:
      let sym = node.sons[namePos].sym
      if sfCompileTime notin sym.flags:
        if sym.skipGenericOwner.kind == skModule and sfCompileTime notin sym.flags:
          if ({sfExportc, sfCompilerProc} * sym.flags == {sfExportc}) or
              (sfExportc in sym.flags and lfExportLib in sym.loc.flags) or
              (sym.kind == skMethod):
            if sym.getBody.kind != nkEmpty or lfDynamicLib in sym.loc.flags:
              discard gen_proc(module, sym)
  of nkParForStmt:
    discard
  of nkState:
    discard
  of nkGotoState:
    discard
  of nkBreakState:
    discard
  of nkStringToCString:
    result = gen_string_to_cstring(module, node)
  of nkCStringToString:
    result = gen_cstring_to_string(module, node)
  else: echo "gen_expr: unknown node kind: ", node.kind

# ------------------------------------------------------------------------------

proc gen_stmt(module: BModule; node: PNode) =
  let val = gen_expr(module, node)
