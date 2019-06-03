# included from "llvm_pass.nim"

proc file_info(module: BModule; node: PNode): string =
  let line = int node.info.line
  let col = int node.info.col
  let index = int node.info.fileIndex
  let file_infos = module.module_list.config.m.fileInfos
  let filename =
    if index in low(file_infos) .. high(file_infos): file_infos[index].shortName
    else: "unk"

  result = "L: " & $line & ", C: " & $col & " | " & filename

# ------------------------------------------------------------------------------

proc `$`(x: ValueRef): string =
  if x == nil:
    result = "nil ValueRef"
  else:
    let str = llvm.printValueToString(x)
    result = $str
    disposeMessage(str)

# ------------------------------------------------------------------------------

template assert_value_type(val: ValueRef; kind: TypeKind) =
  when true:
    assert val != nil
    let typ = llvm.typeOf(val)
    if llvm.getTypeKind(typ) != kind:
      let type_name = llvm.printTypeToString(typ)
      let value_name = llvm.printValueToString(val)
      echo "[!!!!] assert_value_type fail: ", value_name, " type: ", type_name, " expected: ", kind
      echo "[!!!!] ", instantiationInfo()
      disposeMessage(type_name)
      disposeMessage(value_name)
      # assert false

# ------------------------------------------------------------------------------

proc constant(module: BModule; value: int8): ValueRef =
  result = llvm.constInt(module.ll_int8, culonglong value, Bool 1)

proc constant(module: BModule; value: int16): ValueRef =
  result = llvm.constInt(module.ll_int16, culonglong value, Bool 1)

proc constant(module: BModule; value: int32): ValueRef =
  result = llvm.constInt(module.ll_int32, culonglong value, Bool 1)

proc constant(module: BModule; value: int64): ValueRef =
  result = llvm.constInt(module.ll_int64, culonglong value, Bool 1)

proc constant(module: BModule; value: uint8): ValueRef =
  result = llvm.constInt(module.ll_int8, culonglong value, Bool 0)

proc constant(module: BModule; value: uint16): ValueRef =
  result = llvm.constInt(module.ll_int16, culonglong value, Bool 0)

proc constant(module: BModule; value: uint32): ValueRef =
  result = llvm.constInt(module.ll_int32, culonglong value, Bool 0)

proc constant(module: BModule; value: uint64): ValueRef =
  result = llvm.constInt(module.ll_int64, culonglong value, Bool 0)

proc constant_int(module: BModule; value: int64): ValueRef =
  case module.module_list.config.target.intSize:
  of 8: result = constant(module, int64 value)
  of 4: result = constant(module, int32 value)
  else: assert false

proc constant_true(module: BModule): ValueRef =
  result = llvm.constInt(module.ll_bool, culonglong 1, Bool 0)

proc constant_false(module: BModule): ValueRef =
  result = llvm.constInt(module.ll_bool, culonglong 0, Bool 0)

proc constant_nil(module: BModule): ValueRef =
  result = llvm.constNull(module.ll_pointer)

# ------------------------------------------------------------------------------

proc build_generic_cmp(module: BModule; pred: string; lhs, rhs: ValueRef; typ: PType): ValueRef =
  case typ.kind:
  of tyInt .. tyInt64:
    var op: IntPredicate
    case pred:
    of "==": op = IntEq
    of "!=": op = IntNE
    of ">=": op = IntSGE
    of "<=": op = IntSLE
    of ">":  op = IntSGT
    of "<":  op = IntSLT
    else: assert false
    result = llvm.buildICmp(module.ll_builder, op, lhs, rhs, "")
  of tyUint .. tyUInt64, tyEnum, tyChar:
    var op: IntPredicate
    case pred:
    of "==": op = IntEq
    of "!=": op = IntNE
    of ">=": op = IntUGE
    of "<=": op = IntULE
    of ">":  op = IntUGT
    of "<":  op = IntULT
    else: assert false
    result = llvm.buildICmp(module.ll_builder, op, lhs, rhs, "")
  of tyFloat, tyFloat32, tyFloat64:
    var op: RealPredicate
    case pred:
    of "==": op = RealOEQ
    of "!=": op = RealONE
    of ">=": op = RealOGE
    of "<=": op = RealOLE
    of ">":  op = RealOGT
    of "<":  op = RealOLT
    else: assert false
    result = llvm.buildFCmp(module.ll_builder, op, lhs, rhs, "")
  of tyString:
    assert false
  else: assert false

# ------------------------------------------------------------------------------

proc build_i8_to_i1(module: BModule; value: ValueRef): ValueRef =
  assert value != nil
  if llvm.getValueKind(value) == InstructionValueKind:
    # eleminate common zext trunc combo
    if llvm.getInstructionOpcode(value) == llvm.ZExt:
      result = llvm.getOperand(value, 0)
      if llvm.getFirstUse(value) == nil:
        llvm.instructionEraseFromParent(value)
      return
  result = llvm.buildTrunc(module.ll_builder, value, module.ll_bool, "")

proc build_i1_to_i8(module: BModule; value: ValueRef): ValueRef =
  result = llvm.buildZExt(module.ll_builder, value, module.ll_mem_bool, "")

# ------------------------------------------------------------------------------

proc build_field_ptr(module: BModule; struct, index: ValueRef; hint = "struct.index"): ValueRef =
  assert_value_type(struct, PointerTypeKind)
  var indices = [constant(module, 0i32), index]
  result = llvm.buildGEP(module.ll_builder, struct, addr indices[0], 2, hint)

proc build_mtype_field_ptr(module: BModule; struct: ValueRef; typ: PType): ValueRef =
  # get pointer to the object "m_type" field
  assert_value_type(struct, PointerTypeKind)
  assert typ != nil
  var indices: seq[ValueRef]
  indices.add constant(module, int32 0) # remove pointer
  var typ = skipTypes(typ, abstractInstOwned)
  while typ.kind in {tyVar, tyLent, tyPtr, tyRef}:
    typ = skipTypes(typ.lastSon, typedescInst + {tyOwned})
  while typ.kind == tyObject and typ[0] != nil:
    typ = skipTypes(typ[0], skipPtrs)
    indices.add constant(module, int32 0) # super type
  indices.add constant(module, int32 0) # m_type
  result = llvm.buildGEP(module.ll_builder, struct, addr indices[0], cuint len indices, "m_type.ptr")
  assert_value_type(result, PointerTypeKind)

# ------------------------------------------------------------------------------

proc maybe_terminate(module: BModule; target: BasicBlockRef) =
  # try to terminate current block
  if llvm.getBasicBlockTerminator(getInsertBlock(module.ll_builder)) == nil:
    discard llvm.buildBr(module.ll_builder, target)

proc build_entry_alloca(module: BModule; typ: TypeRef; name: cstring): ValueRef =
  # insert `alloca` instruction at function entry point
  let incoming_bb = llvm.getInsertBlock(module.ll_builder)
  let fun         = llvm.getBasicBlockParent(incoming_bb)
  let entry_bb    = llvm.getEntryBasicBlock(fun)
  let first       = llvm.getFirstInstruction(entry_bb)
  if first == nil:
    llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)
  else:
    llvm.positionBuilderBefore(module.ll_builder, first)
  result = llvm.buildAlloca(module.ll_builder, typ, name)
  # restore position
  llvm.positionBuilderAtEnd(module.ll_builder, incoming_bb)

proc build_open_array(module: BModule; ptr_T: PType; data_ptr: ValueRef; length: ValueRef; name: string): ValueRef =
  let ll_ptr_T = type_to_ptr get_type(module, ptr_T)
  var struct_fields = [ll_ptr_T, module.ll_int]
  let ll_type = llvm.structTypeInContext(module.ll_context, addr struct_fields[0], 2, Bool 0) # {T*, i64}
  result = build_entry_alloca(module, ll_type, name) # {T*, i64}*
  let field0 = build_field_ptr(module, result, constant(module, 0i32)) # T**
  let field1 = build_field_ptr(module, result, constant(module, 1i32)) # i64*
  discard llvm.buildStore(module.ll_builder, data_ptr, field0) # T* -> T*
  discard llvm.buildStore(module.ll_builder, length, field1) # i64 -> i64

template build_nil_check(module: BModule; value: ValueRef; code_then, code_else: typed): ValueRef =
  # execute `code_then` if value is not nil
  assert_value_type(value, PointerTypeKind)

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
  let null = llvm.constNull(llvm.typeOf(value))
  let cond = llvm.buildICmp(module.ll_builder, IntNE, value, null, "")
  discard llvm.buildCondBr(module.ll_builder, cond, then_bb, else_bb)

  # nilcheck.then:
  llvm.positionBuilderAtEnd(module.ll_builder, then_bb)

  let value_then = code_then

  let then_inc = llvm.getInsertBlock(module.ll_builder)
  discard llvm.buildBr(module.ll_builder, cont_bb)

  # nilcheck.else:
  llvm.positionBuilderAtEnd(module.ll_builder, else_bb)

  let value_else = code_else

  let else_inc = llvm.getInsertBlock(module.ll_builder)
  discard llvm.buildBr(module.ll_builder, cont_bb)

  # nilcheck.cont
  llvm.positionBuilderAtEnd(module.ll_builder, cont_bb)
  let phi = llvm.buildPhi(module.ll_builder, llvm.typeOf(value_then), "nilcheck")
  var values = [ value_then, value_else ]
  var blocks = [ then_inc, else_inc ]
  llvm.addIncoming(phi, addr values[0], addr blocks[0], 2)
  phi

# ------------------------------------------------------------------------------

type PathNode = object
  index: int
  node: PNode

proc find_field(module: BModule; field: PSym; node: PNode; path: var seq[PathNode]): bool =
  case node.kind:
  of nkRecList:
    path.add PathNode(index: 0, node: node)
    for item in node:
      if find_field(module, field, item, path):
        return true
      else:
        inc path[^1].index
    path.del(path.high)
  of nkRecCase:
    if node[0].sym.id == field.id:
      return true
    inc path[^1].index
    for item in node.sons[1 .. ^1]:
      if item.kind in {nkOfBranch, nkElse}:
        if find_field(module, field, item.lastSon, path):
          return true
  of nkSym:
    if node.sym.id == field.id:
      return true
  else:
    discard

proc build_field_access(module: BModule; object_type: PType; object_value: ValueRef; field: PSym): ValueRef =
  # returns pointer to object field

  assert object_type != nil
  assert object_type.kind == tyObject, $object_type.kind
  assert field != nil

  var path: seq[PathNode]
  var found = false
  var current = object_type
  while true:
    assert current.kind == tyObject
    set_len(path, 0)
    let super = if current.sons.len == 0: nil else: current[0]
    let found = find_field(module, field, current.n, path)
    if found or (current == nil): break
    current = skipTypes(super, abstractPtrs)

  assert path.len > 0

  if current.sons.len != 0 and current[0] != nil:
    inc path[0].index # skip superclass field

  if current.sons.len == 0:
    if (tfFinal notin current.flags) and (current.sym != nil and sfPure notin current.sym.flags):
      inc path[0].index # skip m_type

  let object_adr =
    if current != object_type:
      llvm.buildBitCast(module.ll_builder, object_value, type_to_ptr get_type(module, current), "bfa.super_cast")
    else:
      object_value

  var indices = [ constant(module, 0i32), constant(module, path[0].index.int32) ]
  result = llvm.buildGEP(module.ll_builder, object_adr, addr indices[0], cuint len indices, "")

  for i in 1 .. path.high:
    let brach_type = llvm.pointerType(get_object_case_branch_type(module, path[i].node), 0)

    var indices = [ constant(module, 0i32), constant(module, path[i].index.int32) ]
    let bitcast = llvm.buildBitCast(module.ll_builder, result, brach_type, "branch_cast")
    result = llvm.buildGEP(module.ll_builder, bitcast, addr indices[0], cuint len indices, "")

# Intrisics --------------------------------------------------------------------

proc call_intrisic(module: BModule; name: string; typ: TypeRef; args: openarray[ValueRef]): ValueRef =
  var fn = llvm.getNamedFunction(module.ll_module, name)
  if fn == nil:
    fn = llvm.addFunction(module.ll_module, name, typ)
  result = llvm.buildCall(module.ll_builder, fn, unsafe_addr args[0], cuint len args, "")

proc build_call_memcpy(module: BModule; dst, src: ValueRef; len: int64) =
  case module.module_list.config.target.intSize:
  of 8:
    var args = [ dst, src, constant(module, len), constant_false(module) ]
    discard call_intrisic(module, "llvm.memcpy.p0i8.p0i8.i64", module.ll_memcpy64, args)
  of 4:
    var args = [ dst, src, constant(module, int32 len), constant_false(module) ]
    discard call_intrisic(module, "llvm.memcpy.p0i8.p0i8.i32", module.ll_memcpy32, args)
  else: assert(false)

proc build_call_memset(module: BModule; dst: ValueRef; val: int8; len: int64) =
  case module.module_list.config.target.intSize:
  of 8:
    var args = [ dst, constant(module, val), constant(module, len), constant_false(module) ]
    discard call_intrisic(module, "llvm.memset.p0i8.i64", module.ll_memset64, args)
  of 4:
    var args = [ dst, constant(module, val), constant(module, int32 len), constant_false(module) ]
    discard call_intrisic(module, "llvm.memset.p0i8.i32", module.ll_memset32, args)
  else: assert(false)

proc build_call_setjmp(module: BModule; buff: ValueRef): ValueRef =
  assert_value_type(buff, PointerTypeKind)
  var args = [ buff ]
  #result = call_intrisic(module, "llvm.eh.sjlj.setjmp", module.ll_setjmp, args)
  result = call_intrisic(module, "_setjmp", module.ll_setjmp, args)

proc build_call_longjmp(module: BModule; buff: ValueRef) =
  assert_value_type(buff, PointerTypeKind)
  var args = [buff]
  discard call_intrisic(module, "llvm.eh.sjlj.longjmp", module.ll_longjmp, args)

# ------------------------------------------------------------------------------

proc add_function_attr(module: BModule; fun: ValueRef; attr: AttributeRef) =
  llvm.addAttributeAtIndex(fun, 0xFFFF_FFFFu32, attr)
