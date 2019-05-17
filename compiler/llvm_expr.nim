import ast, types
import llvm_data, llvm_type, llvm_aux
import llvm_dll as llvm

from lineinfos import TLineInfo

import astalgo

proc gen_stmt*(module: BModule; node: PNode)
proc gen_expr*(module: BModule; node: PNode): ValueRef
proc gen_expr_lvalue*(module: BModule; node: PNode): ValueRef
proc gen_copy*(module: BModule; lhs, rhs: ValueRef; typ: PType)
proc gen_default_init*(module: BModule; typ: PType; alloca: ValueRef)

import llvm_proc

# ------------------------------------------------------------------------------

proc gen_copy*(module: BModule; lhs, rhs: ValueRef; typ: PType) =
  assert lhs != nil
  assert rhs != nil
  assert typ != nil
  #echo "gen_copy typ: ", typ.kind
  case typ.kind:
  of tyObject, tyArray, tyTuple:
    assert_value_type(lhs, PointerTypeKind)
    assert_value_type(rhs, PointerTypeKind)
    let dst = llvm.buildBitCast(module.ll_builder, lhs, module.ll_pointer, "")
    let src = llvm.buildBitCast(module.ll_builder, rhs, module.ll_pointer, "")
    let size = get_type_size(module, typ)
    build_call_memcpy(module, dst, src, size)
  of tyInt .. tyInt64, tyUInt .. tyUInt64:
    discard llvm.buildStore(module.ll_builder, rhs, lhs)
  of tyFloat, tyFloat32, tyFloat64:
    discard llvm.buildStore(module.ll_builder, rhs, lhs)
  of tyPtr, tyPointer, tyCString:
    discard llvm.buildStore(module.ll_builder, rhs, lhs)
  of tyBool, tyChar, tyEnum, tyRange:
    discard llvm.buildStore(module.ll_builder, rhs, lhs)
  of tyString:
    discard llvm.buildStore(module.ll_builder, rhs, lhs)
  of tyRef:
    discard llvm.buildStore(module.ll_builder, rhs, lhs)
  of tySequence:
    discard llvm.buildStore(module.ll_builder, rhs, lhs)
  of tySet:
    let size = get_type_size(module, typ)
    case size:
    of 1, 2, 4, 8:
      discard llvm.buildStore(module.ll_builder, rhs, lhs)
    else:
      let dst = llvm.buildBitCast(module.ll_builder, lhs, module.ll_pointer, "")
      let src = llvm.buildBitCast(module.ll_builder, rhs, module.ll_pointer, "")
      build_call_memcpy(module, dst, src, size)
  of tyProc:
    discard llvm.buildStore(module.ll_builder, rhs, lhs)
  else:
    #echo "gen_copy lhs: ", lhs, ", rhs: ", rhs
    assert(false)

proc gen_default_init*(module: BModule; typ: PType; alloca: ValueRef) =
  assert typ != nil
  assert alloca != nil
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

# - Literals -------------------------------------------------------------------

proc gen_nil_lit(module: BModule; node: PNode): ValueRef =
  let ll_type = get_type(module, node.typ)
  result = llvm.constNull(ll_type)

proc gen_int_lit(module: BModule; node: PNode): ValueRef =
  let ll_type = get_type(module, node.typ)
  result = llvm.constInt(ll_type, culonglong node.intVal, Bool 1)

proc gen_uint_lit(module: BModule; node: PNode): ValueRef =
  let ll_type = get_type(module, node.typ)
  result = llvm.constInt(ll_type, culonglong node.intVal, Bool 0)

proc gen_char_lit(module: BModule; node: PNode): ValueRef =
  let ll_type = get_type(module, node.typ)
  result = llvm.constInt(ll_type, culonglong node.intVal, Bool 0)

proc gen_float_lit(module: BModule; node: PNode): ValueRef =
  let ll_type = get_type(module, node.typ)
  result = llvm.constReal(ll_type, node.floatVal)

proc gen_object_lit(module: BModule; node: PNode): ValueRef =
  # todo: constant
  let ll_type = get_type(module, node.typ)
  let alloca = build_entry_alloca(module, ll_type, "object_literal")
  let adr = llvm.buildBitCast(module.ll_builder, alloca, module.ll_pointer, "")
  let size = get_type_size(module, node.typ)
  build_call_memset(module, adr, 0, size)
  result = alloca
  for i in 1 ..< node.len:
    let field = node[i]
    let lhs = build_field_access(module, node.typ, alloca, field[0].sym)
    let rhs = gen_expr(module, field[1])
    gen_copy(module, lhs, rhs, field[0].typ)

proc gen_array_lit(module: BModule; node: PNode): ValueRef =
  # todo: constant
  let ll_type = get_type(module, node.typ)
  # allocate memory for literal
  result = build_entry_alloca(module, ll_type, "array_literal")
  # zero memory
  let adr = llvm.buildBitCast(module.ll_builder, result, module.ll_pointer, "")
  let size = get_type_size(module, node.typ)
  build_call_memset(module, adr, 0, size)
  # get pointer to first element
  var indices = [
    llvm.constInt(module.ll_int, culonglong 0, Bool 0),
    llvm.constInt(module.ll_int, culonglong 0, Bool 0)]
  var lhs = llvm.buildGEP(module.ll_builder, result, addr indices[0], cuint len indices, "")
  # copy each element
  for index, item in node:
    # get pointer to next element
    if index != 0:
      var indices = [ llvm.constInt(module.ll_int32, culonglong 1, Bool 0) ]
      lhs = llvm.buildGEP(module.ll_builder, lhs, addr indices[0], cuint len indices, "")
    # copy
    let rhs = gen_expr(module, item)
    gen_copy(module, lhs, rhs, item.typ)

proc gen_int_set_lit(module: BModule; node: PNode; size: int): ValueRef =
  let ll_type = get_type(module, node.typ)
  let accum = build_entry_alloca(module, ll_type, "curly.accum")
  gen_default_init(module, node.typ, accum)

  for item in node:
    if item.kind == nkRange:
      discard
    else:
      # accum = accum or (1 shl value)
      let value = gen_expr(module, item)
      let one = llvm.constInt(ll_type, culonglong 1, Bool 0)
      let bit = llvm.buildShl(module.ll_builder, one, value, "curly.bit")
      let accum_value = llvm.buildLoad(module.ll_builder, accum, "curly.accum.load")
      let merged = llvm.buildOr(module.ll_builder, accum_value, bit, "curly.merged")
      discard llvm.buildStore(module.ll_builder, merged, accum)

  result = llvm.buildLoad(module.ll_builder, accum, "curly.load")

proc gen_array_set_lit(module: BModule; node: PNode; size: int): ValueRef =
  assert false

proc gen_set_lit(module: BModule; node: PNode): ValueRef =
  #echo "gen_set_lit:"
  let size = int get_type_size(module, node.typ)

  if size <= 8:
    result = gen_int_set_lit(module, node, size)
  else:
    result = gen_array_set_lit(module, node, size)

proc gen_str_lit(module: BModule; node: PNode): ValueRef =
  if node.typ.kind == tyCString:
    let typ = llvm.arrayType(module.ll_char, cuint len(node.strVal) + 1)

    let initializer = llvm.constStringInContext(module.ll_context, node.strVal, cuint len node.strVal, Bool 0)
    let global = llvm.addGlobal(module.ll_module, typ, "literal.cstring")
    llvm.setInitializer(global, initializer)

    var indices = [constant(module, 0i32), constant(module, 0i32)]
    result = llvm.buildGEP(module.ll_builder, global, addr indices[0], cuint len indices, "")
    llvm.setGlobalConstant(global, Bool 1)
    llvm.setLinkage(global, PrivateLinkage)
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

    result = llvm.buildBitCast(module.ll_builder, global, get_nim_string_type(module), "")
  else:
    assert false

# ------------------------------------------------------------------------------

proc fix_literal(module: BModule; node: PNode; lhs, rhs: ValueRef): (ValueRef, ValueRef) =
  # literals sometimes getting wrong types...

  result[0] = lhs
  result[1] = rhs

  if node[2].kind in {nkIntLit}:
    result[1] = convert_scalar(module, rhs, llvm.typeOf(lhs), is_signed_type(node[1].typ))
    #echo "fixed literal: ", node.kind, " - ", node[1].kind

  if node[1].kind in {nkIntLit}:
    result[0] = convert_scalar(module, lhs, llvm.typeOf(rhs), is_signed_type(node[2].typ))
    #echo "fixed literal: ", node.kind, " - ", node[2].kind

proc gen_logic_or_and(module: BModule; node: PNode; op: TMagic): ValueRef =
  let incoming_bb = llvm.getInsertBlock(module.ll_builder)
  let fun = llvm.getBasicBlockParent(incoming_bb)
  let end_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "or.end")
  var rhs_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "or.rhs")
  llvm.moveBasicBlockAfter(rhs_bb, incoming_bb)
  llvm.moveBasicBlockAfter(end_bb, rhs_bb)

  # evaluate lhs first
  llvm.positionBuilderAtEnd(module.ll_builder, incoming_bb)
  let lhs = build_i8_to_i1(module, gen_expr(module, node[1]))
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
  let rhs = build_i8_to_i1(module, gen_expr(module, node[2]))
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

proc gen_magic_echo(module: BModule; node: PNode) =
  #for i in 1 ..< node.len:
  #  let arg_node = node[i]
  discard

proc gen_magic_inc(module: BModule; node: PNode) =
  let adr = gen_expr_lvalue(module, node[1])
  let xincrement = gen_expr(module, node[2])
  let xvalue = llvm.buildLoad(module.ll_builder, adr, "")
  let (value, increment) = fix_literal(module, node, xvalue, xincrement)
  let new_value = llvm.buildAdd(module.ll_builder, value, increment, "inc")
  discard llvm.buildStore(module.ll_builder, new_value, adr)

proc gen_magic_dec(module: BModule; node: PNode) =
  let adr = gen_expr_lvalue(module, node[1])
  let xincrement = gen_expr(module, node[2])
  let xvalue = llvm.buildLoad(module.ll_builder, adr, "")
  let (value, increment) = fix_literal(module, node, xvalue, xincrement)
  let new_value = llvm.buildSub(module.ll_builder, value, increment, "dec")
  discard llvm.buildStore(module.ll_builder, new_value, adr)

proc gen_magic_length(module: BModule; node: PNode): ValueRef =
  #echo "gen_magic_length"
  let sym_node = if node[1].kind == nkHiddenDeref: node[1][0] else: node[1]
  #debug sym_node
  case sym_node.typ.kind:
  of tyOpenArray:
    let struct = gen_expr_lvalue(module, sym_node)
    assert_value_type(struct, PointerTypeKind)
    let len_field = build_field_ptr(module, struct, constant(module, 1i32), "openarray.len")
    assert_value_type(len_field, PointerTypeKind)
    result = llvm.buildLoad(module.ll_builder, len_field, "magic.openarray.len")
    assert_value_type(result, IntegerTypeKind)
  else:
    assert false

include llvm_magic_set
include llvm_magic_string

proc gen_magic_eq_proc(module: BModule; node: PNode): ValueRef =
  if node[1].typ.callConv == ccClosure:
    assert false
  else:
    let lhs = gen_expr(module, node[1])
    let rhs = gen_expr(module, node[2])
    assert lhs != nil, $node[1].kind
    assert rhs != nil, $node[2].kind
    result = llvm.buildICmp(module.ll_builder, IntEQ, lhs, rhs, "eq.proc")

proc gen_magic_expr(module: BModule; node: PNode; op: TMagic): ValueRef =

  proc unary(prc: UnaryProc): ValueRef =
    let lhs = gen_expr(module, node[1])
    assert lhs != nil
    result = prc(module.ll_builder, lhs, "")

  proc binary(prc: BinaryProc): ValueRef =
    let xlhs = gen_expr(module, node[1])
    let xrhs = gen_expr(module, node[2])
    let (lhs, rhs) = fix_literal(module, node, xlhs, xrhs)
    result = prc(module.ll_builder, lhs, rhs, "")

  proc cmp_int(pred: IntPredicate): ValueRef =
    let xlhs = gen_expr(module, node[1])
    let xrhs = gen_expr(module, node[2])
    let (lhs, rhs) = fix_literal(module, node, xlhs, xrhs)
    result = llvm.buildICmp(module.ll_builder, pred, lhs, rhs, "")
    result = build_i1_to_i8(module, result)

  proc unary_minus(): ValueRef =
    # -value
    let value = gen_expr(module, node[1])
    result = llvm.buildNeg(module.ll_builder, value, "unary_minus")

  proc abs_int(): ValueRef =
    # if x >= 0 x else: -x
    let pred = if is_signed_type(node[1].typ): llvm.IntSGE else: llvm.IntUGe
    let value = gen_expr(module, node[1])
    let zero = llvm.constInt(llvm.typeOf(value), 0, Bool 1)
    let neg_value = llvm.buildNeg(module.ll_builder, value, "abs.neg_value")
    let cond = llvm.buildICmp(module.ll_builder, pred, value, zero, "abs.cmp")
    result = llvm.buildSelect(module.ll_builder, cond, value, neg_value, "abs.result")

  proc min_int(): ValueRef =
    # if x <= y: x else: y
    let pred = if is_signed_type(node[1].typ): llvm.IntSLE else: llvm.IntULE
    let x = gen_expr(module, node[1])
    let y = gen_expr(module, node[2])
    let cond = llvm.buildICmp(module.ll_builder, pred, x, y, "min.cond")
    result = llvm.buildSelect(module.ll_builder, cond, x, y, "min.result")

  proc max_int(): ValueRef =
    # if x >= y: x else: y
    let pred = if is_signed_type(node[1].typ): llvm.IntSGE else: llvm.IntUGE
    let x = gen_expr(module, node[1])
    let y = gen_expr(module, node[2])
    let cond = llvm.buildICmp(module.ll_builder, pred, x, y, "max.cond")
    result = llvm.buildSelect(module.ll_builder, cond, x, y, "max.result")

  proc float_binary(prc: BinaryProc): ValueRef =
    let lhs = gen_expr(module, node[1])
    let rhs = gen_expr(module, node[2])
    assert lhs != nil
    assert rhs != nil
    result = prc(module.ll_builder, lhs, rhs, "")

  proc abs_float(): ValueRef =
    # if x >= 0 x else: -x
    let value = gen_expr(module, node[1])
    let zero = llvm.constReal(llvm.typeOf(value), 0)
    let neg_value = llvm.buildNeg(module.ll_builder, value, "abs.neg_value")
    let cond = llvm.buildFCmp(module.ll_builder, RealOGE, value, zero, "abs.cmp")
    result = llvm.buildSelect(module.ll_builder, cond, value, neg_value, "abs.result")

  proc min_float(): ValueRef =
    # if x <= y: x else: y
    let x = gen_expr(module, node[1])
    let y = gen_expr(module, node[2])
    let cond = llvm.buildFCmp(module.ll_builder, llvm.RealOLE, x, y, "")
    result = llvm.buildSelect(module.ll_builder, cond, x, y, "")

  proc max_float(): ValueRef =
    # if x >= y: x else: y
    let x = gen_expr(module, node[1])
    let y = gen_expr(module, node[2])
    let cond = llvm.buildFCmp(module.ll_builder, llvm.RealOGE, x, y, "")
    result = llvm.buildSelect(module.ll_builder, cond, x, y, "")

  proc cmp_float(pred: RealPredicate): ValueRef =
    let lhs = gen_expr(module, node[1])
    let rhs = gen_expr(module, node[2])
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
  of mUnaryPlusI: result = gen_expr(module, node[1])
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
  of mExit: result = gen_call_runtime_proc(module, node)
  #of mOrd: discard
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
  # strings
  of mLengthStr: result = gen_magic_length_str(module, node)
  of mNewString: result = gen_magic_new_string(module, node)
  # of mNewStringOfCap:
  # of mSetLengthStr:
  # of mCopyStr:
  # of mCopyStrLast:
  # of mConStrStr:
  # of mAppendStrCh:
  # of mAppendStrStr:
  # of mAppendSeqElem:
  # of mEqStr:
  # of mLeStr:
  # of mLtStr:
  # of mIsNil:
  # of mIntToStr:
  # of mInt64ToStr:
  # of mBoolToStr:
  # of mCharToStr:
  # of mFloatToStr:
  # of mCStrToStr:
  # of mStrToStr:
  # of mEnumToStr:
  else: echo "unknown magic: ", op

# Procedure Types --------------------------------------------------------------

proc gen_call(module: BModule; node: PNode): ValueRef =
  if node[0].kind == nkSym and node[0].sym.magic != mNone:
    result = gen_magic_expr(module, node, node[0].sym.magic)
  else:
    result = gen_call_expr(module, node)

# - Statements -----------------------------------------------------------------

proc gen_asgn(module: BModule; node: PNode) =
  let lhs = gen_expr_lvalue(module, node[0])
  let rhs = gen_expr(module, node[1])
  assert lhs != nil, $node[0].kind
  assert rhs != nil, $node[1].kind
  gen_copy(module, lhs, rhs, node[0].typ)

proc gen_var_prototype(module: BModule; node: PNode): ValueRef =
  let sym = node.sym
  let typ = node.sym.typ
  let ll_type = get_type(module, typ)
  let name = mangle_global_var_name(module, sym)
  let global = llvm.addGlobal(module.ll_module, ll_type, name)

  echo "%% --------------------------------------------------"
  echo "%% gen_var_prototype: ", sym.name.s
  echo "%% mangled name     : ", name
  echo "%% llvm type        : ", ll_type
  echo "%% --------------------------------------------------"

  llvm.setLinkage(global, ExternalLinkage)
  result = global

proc gen_single_var(module: BModule; node: PNode) =

  let sym = node[0].sym

  proc gen_global =
    let typ = node[0].sym.typ
    let ll_type = get_type(module, typ)
    let name = mangle_global_var_name(module, sym)

    echo "☺☺ --------------------------------------------------"
    echo "☺☺ gen_global       : ", sym.name.s
    echo "☺☺ mangled name     : ", name
    echo "☺☺ id               : ", sym.id
    echo "☺☺ flags            : ", sym.flags
    echo "☺☺ --------------------------------------------------"

    assert ll_type != nil
    let adr = llvm.addGlobal(module.ll_module, ll_type, name)
    llvm.setInitializer(adr, llvm.constNull(ll_type))

    #llvm.setLinkage(adr, ExternalLinkage)
    llvm.setVisibility(adr, DefaultVisibility)

    # initialize global variable
    if node[2].kind == nkEmpty:
      gen_default_init(module, typ, adr)
    else:
      let value = gen_expr(module, node[2])
      assert value != nil
      assert adr != nil
      gen_copy(module, adr, value, node[2].typ)
    module.add_value(sym, adr)

  proc gen_local =
    let typ = node[0].sym.typ
    let ll_type = get_type(module, typ)
    let name = mangle_local_name(module, sym)
    assert ll_type != nil
    let adr = build_entry_alloca(module, ll_type, name)
    # initialize variable
    if node[2].kind == nkEmpty:
      gen_default_init(module, typ, adr)
    else:
      # initialize with exrp
      let value = gen_expr(module, node[2])
      assert value != nil, $node[2].kind
      assert adr != nil
      gen_copy(module, adr, value, node[2].typ)
    module.add_value(sym, adr)

  if (sfCompileTime notin sym.flags) and (lfNoDecl notin sym.loc.flags):
    if sfGlobal in sym.flags:
      gen_global()
    else:
      gen_local()

proc gen_var_section(module: BModule; node: PNode) =
  for def in node.sons:
    if def.kind == nkCommentStmt: continue
    if def.kind == nkIdentDefs:
      let id = def[0]
      let value = def[2]
      gen_single_var(module, def)

# - Control Flow ---------------------------------------------------------------

proc gen_if(module: BModule; node: PNode): ValueRef =
  assert(module != nil)
  assert(node != nil)

  #[
  if a: 1
  elif b: 2
  elif c: 3
  else: 4

  entry:
    cond a
    br a, then, elif1
  then:
    1
    br post
  elif1:
    cond b
    br b, then1, elif2
  then1:
    2
    br post
  elif2:
    cond c
    br c, then2, else
  then2:
    3
    br post
  else:
    4
    br post
  post:
  ]#

  if not isEmptyType(node.typ): # if expr
    let ll_type = get_type(module, node.typ)
    result = build_entry_alloca(module, ll_type, "if.expr.result")

  let entry_bb = llvm.getInsertBlock(module.ll_builder)
  let fun = llvm.getBasicBlockParent(entry_bb)

  let post_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "if.post")

  llvm.moveBasicBlockAfter(post_bb, entry_bb)

  var else_bb: BasicBlockRef = nil

  for i in countdown(sonsLen(node) - 1, 0):
    let it = node.sons[i]
    if it.kind in  {nkElifBranch, nkElifExpr}:
      # nkElifBranch
      var elif_bb: BasicBlockRef = nil
      var cond_bb: BasicBlockRef = entry_bb
      if i != 0:
        # generate label for elif branch
        elif_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "if.elif")
        llvm.moveBasicBlockAfter(elif_bb, entry_bb)
        cond_bb = elif_bb

      let then_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "if.true")
      llvm.moveBasicBlockAfter(then_bb, cond_bb)

      # *** emit cond ***

      llvm.positionBuilderAtEnd(module.ll_builder, cond_bb)
      let cond = build_i8_to_i1(module, gen_expr(module, it[0]))

      if else_bb == nil:
        # else branch missing
        discard llvm.buildCondBr(module.ll_builder, cond, then_bb, post_bb)
      else:
        discard llvm.buildCondBr(module.ll_builder, cond, then_bb, else_bb)

      llvm.positionBuilderAtEnd(module.ll_builder, then_bb)

      # *** emit then branch code ***

      let value = gen_expr(module, it.sons[1])
      if result != nil: gen_copy(module, result, value, node.typ)

      # terminate basic block if needed
      maybe_terminate(module, post_bb)

      else_bb = elif_bb
    elif it.kind in {nkElse, nkElseExpr}:
      # nkElse
      else_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "if.false")
      llvm.moveBasicBlockAfter(else_bb, entry_bb)
      llvm.positionBuilderAtEnd(module.ll_builder, else_bb)

      # *** emit else branch code ***

      let value = gen_expr(module, it.sons[0])
      if result != nil: gen_copy(module, result, value, node.typ)

      # terminate basic block if needed
      maybe_terminate(module, post_bb)

  llvm.positionBuilderAtEnd(module.ll_builder, post_bb)

  if result != nil and node.typ.kind notin {tyObject, tyTuple, tyArray}:
    result = llvm.buildLoad(module.ll_builder, result, "")

proc gen_while(module: BModule; node: PNode) =
  # todo: transf pass for some reason wraps loop in named block and replaces
  # `continue` with `break`

  #[

  while cond:
    1

    br cond_bb // terminate
  cond_bb:
    br cond, loop_bb, exit_bb
  loop_bb:
    1
    br cond
  exit_bb:

  ]#

  let entry_bb = llvm.getInsertBlock(module.ll_builder)
  let fun = llvm.getBasicBlockParent(entry_bb)

  let cond_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "while.cond")
  llvm.moveBasicBlockAfter(cond_bb, entry_bb)
  let loop_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "while.loop")
  llvm.moveBasicBlockAfter(loop_bb, cond_bb)
  let exit_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "while.exit")
  llvm.moveBasicBlockAfter(exit_bb, loop_bb)

  llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)

  maybe_terminate(module, cond_bb)

  llvm.positionBuilderAtEnd(module.ll_builder, cond_bb)
  # emit condition
  let cond = build_i8_to_i1(module, gen_expr(module, node[0]))
  discard llvm.buildCondBr(module.ll_builder, cond, loop_bb, exit_bb)

  llvm.positionBuilderAtEnd(module.ll_builder, loop_bb)
  # emit loop body
  gen_stmt(module, node[1])

  maybe_terminate(module, cond_bb)

  llvm.positionBuilderAtEnd(module.ll_builder, exit_bb)

proc gen_break(module: BModule; node: PNode) =
  var break_target: BasicBlockRef
  if node[0].kind == nkEmpty:
    # unnamed break
    break_target = module.top_scope.break_target
  else:
    # named break
    let sym = node[0].sym
    for scope in module.lookup():
      if scope.break_name == sym.id:
        break_target = scope.break_target
        break

  assert(break_target != nil)
  discard llvm.buildBr(module.ll_builder, break_target)

proc gen_return(module: BModule; node: PNode) =
  let scope = module.proc_scope()
  discard llvm.buildBr(module.ll_builder, scope.return_target)

proc gen_block(module: BModule; node: PNode): ValueRef =
  # todo: named break may introduce basic blocks without predecessors

  module.open_scope()

  let block_name = if node[0].kind != nkEmpty: node[0].sym.name.s else: "block"

  let entry_bb = llvm.getInsertBlock(module.ll_builder)
  let fun = llvm.getBasicBlockParent(entry_bb)

  let exit_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "exit." & block_name)
  llvm.moveBasicBlockAfter(exit_bb, entry_bb)

  module.top_scope.break_target = exit_bb
  if node[0].kind != nkEmpty:
    # named block
    module.top_scope.break_name = node[0].sym.id

  llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)
  result = gen_expr(module, node[1])

  maybe_terminate(module, exit_bb)

  llvm.positionBuilderAtEnd(module.ll_builder, exit_bb)

  module.close_scope()

proc gen_try(module: BModule; node: PNode) =
  echo "> gen try stmt"
  module.open_scope()

  let entry_bb = llvm.getInsertBlock(module.ll_builder)
  let fun = llvm.getBasicBlockParent(entry_bb)

  for son in node:
    echo "> kind: ", son.kind
    if son.kind == nkExceptBranch:
      discard
    elif son.kind == nkFinally:
      discard
    else:
      discard

  module.close_scope()
  echo "> try end"

proc gen_raise(module: BModule; node: PNode) =
  discard

proc gen_discard(module: BModule; node: PNode) =
  if node[0].kind != nkEmpty:
    discard gen_expr(module, node[0])

proc gen_stmt_list(module: BModule; node: PNode) =
  for son in node.sons:
    gen_stmt(module, son)

proc gen_case_stmt_generic(module: BModule; node: PNode) =
  #[

  case x:
  of 1: discard 1
  of 2: discard 2
  of 3: discard 3
  else: discard 4

  entry:
    .....
    lhs
    br lhs == rhs case.branch1 case.cond2
  case.cond2:
    br lhs == rhs case.branch2 case.cond3
  case.cond3:
    br lhs == rhs case.branch3 case.cond4
  case.cond4:
    br case.else
  case.branch1:
    discard 1
    br case.exit
  case.branch2:
    discard 2
    br case.exit
  case.branch3:
    discard 3
    br case.exit
  case.else:
    discard 4
    br case.exit
  case.exit:
    .....
  ]#

  let entry_bb = llvm.getInsertBlock(module.ll_builder)
  let fun = llvm.getBasicBlockParent(entry_bb)
  let exit_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "case.exit")

  llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)

  let lhs = gen_expr(module, node[0])

  var basic_blocks: seq[BasicBlockRef]

  for branch in node.sons[1 .. ^1]:
    let name = if branch.kind == nkOfBranch: "case.branch" else: "case.else"
    let branch_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, name)
    llvm.positionBuilderAtEnd(module.ll_builder, branch_bb)
    # generate branch code
    let val = gen_expr(module, branch.lastSon)
    maybe_terminate(module, exit_bb)
    basic_blocks.add(branch_bb)

  llvm.moveBasicBlockAfter(exit_bb, llvm.getInsertBlock(module.ll_builder))

  llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)
  for i, branch in node.sons[1 .. ^1]:
    let branch_bb = basic_blocks[i]
    if branch.kind != nkElse:
      for cond in branch.sons[0 ..< ^1]:
        var cmp_result: ValueRef

        if cond.kind == nkRange:
          # range cond
          # lhs >= range_lo and lhs <= range_hi
          let range_lo = gen_expr(module, cond[0])
          let range_hi = gen_expr(module, cond[1])
          let c1 = build_generic_cmp(module, ">=", lhs, range_lo, node[0].typ)
          let c2 = build_generic_cmp(module, "<=", lhs, range_hi, node[0].typ)
          cmp_result = build_i8_to_i1(module, llvm.buildAnd(module.ll_builder, c1, c2, ""))
        else:
          # scalar cond
          let rhs =
            if cond.kind in {nkIntLit}: # literal may get wrong type...
              convert_scalar(module, gen_expr(module, cond), llvm.typeOf(lhs), is_signed_type(node[0].typ))
            else:
              gen_expr(module, cond)

          let cmp = build_generic_cmp(module, "==", lhs, rhs, node[0].typ)
          cmp_result = build_i8_to_i1(module, cmp)

        let save_bb = llvm.getInsertBlock(module.ll_builder)
        let cond_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "case.cond")
        llvm.moveBasicBlockAfter(cond_bb, save_bb)
        llvm.positionBuilderAtEnd(module.ll_builder, save_bb)
        discard llvm.buildCondBr(module.ll_builder, cmp_result, branch_bb, cond_bb)
        llvm.positionBuilderAtEnd(module.ll_builder, cond_bb)
    else:
      maybe_terminate(module, branch_bb)

  llvm.positionBuilderAtEnd(module.ll_builder, exit_bb)

proc gen_case_stmt_ordinal(module: BModule; node: PNode) =
  assert false, "todo"
  #[
  let entry_bb = llvm.getInsertBlock(module.ll_builder)
  let fun = llvm.getBasicBlockParent(entry_bb)
  let exit_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "case.exit")

  llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)

  let lhs = gen_expr(module, node[0])

  var basic_blocks: seq[BasicBlockRef]

  for branch in node.sons[1 .. ^1]:
    let name = if branch.kind == nkOfBranch: "case.branch" else: "case.else"
    let branch_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, name)
    llvm.positionBuilderAtEnd(module.ll_builder, branch_bb)
    # generate branch code
    let val = gen_expr(module, branch.lastSon)
    maybe_terminate(module, exit_bb)
    basic_blocks.add(branch_bb)

  llvm.moveBasicBlockAfter(exit_bb, llvm.getInsertBlock(module.ll_builder))
  ]#

proc gen_case_stmt_string(module: BModule; node: PNode) =
  assert false, "todo"

proc gen_case_stmt(module: BModule; node: PNode) =
  case node[0].typ.kind:
  of tyString:
    gen_case_stmt_string(module, node)
  of tyFloat .. tyFloat128:
    gen_case_stmt_generic(module, node)
  else:
    if node[0].kind == nkSym and sfGoto in node[0].sym.flags:
      assert false, "todo"
    else:
      gen_case_stmt_generic(module, node)
      #gen_case_stmt_ordinal(module, node) # todo

# ------------------------------------------------------------------------------

proc gen_stmt_list_expr(module: BModule; node: PNode): ValueRef =
  for i in 0 .. node.len - 2:
    gen_stmt(module, node[i])
  result = gen_expr(module, node.lastSon)

# ------------------------------------------------------------------------------

proc gen_sym_const(module: BModule; sym: PSym): ValueRef =
  result = module.get_value(sym)
  if result == nil:
    case sym.typ.kind:
    of tyArray:
      let elem_type = get_type(module, elemType(sym.typ))
      let elem_count = cuint lengthOrd(module.module_list.config, sym.typ)
      var items: seq[ValueRef]
      for item in sym.ast:
        items.add gen_expr(module, item)
      let data = llvm.constArray(elem_type, addr items[0], cuint elem_count)
      result = llvm.addGlobal(module.ll_module, llvm.typeOf(data), "constant")
      llvm.setGlobalConstant(result, Bool 1)
      llvm.setInitializer(result, data)
      module.add_value(sym, result)
    else:
      assert false

proc gen_sym_expr_lvalue(module: BModule; node: PNode): ValueRef =
  #echo "gen_sym_expr_lvalue kind: ", node.sym.kind

  case node.sym.kind:
  of skVar, skForVar, skLet, skResult:
    result = module.get_value(node.sym)
    if sfGlobal in node.sym.flags and result == nil:
      result = gen_var_prototype(module, node)
  of skParam:
    result = module.get_value(node.sym)
  of skConst:
    result = gen_sym_const(module, node.sym)
  else:
    echo "fail gen_sym_expr_lvalue: ", node.sym.kind

  assert result != nil, (block: (debug(node); "rip"))
  assert_value_type(result, PointerTypeKind)

proc gen_sym_expr(module: BModule; node: PNode): ValueRef =
  #echo "gen_sym_expr kind: ", node.kind, " sym.kind: ", node.sym.kind

  case node.sym.kind:
  of skProc, skConverter, skIterator, skFunc:
    if sfCompileTime in node.sym.flags: module.ice("attempt to generate code for compile time proc")
    result = gen_proc(module, node.sym)
  of skConst:
    result = gen_sym_const(module, node.sym)
  of skVar, skForVar, skResult, skLet, skParam:
    let alloca = gen_sym_expr_lvalue(module, node)
    case node.sym.typ.kind:
    of tyObject, tyArray, tyTuple:
      result = alloca
    else:
      result = llvm.buildLoad(module.ll_builder, alloca, "gen_sym_expr.deref")
  # of skMethod: discard
  # of skEnumField: discard
  # of skTemp: discard
  else:
    echo "gen_sym_expr: unknown symbol kind: ", node.sym.kind

# ------------------------------------------------------------------------------

proc gen_dot_expr_lvalue(module: BModule; node: PNode): ValueRef =
  let lhs = gen_expr_lvalue(module, node[0])

  let field = node[1].sym
  let object_type = node[0].typ

  if node[0].typ.kind == tyObject:
    result = build_field_access(module, object_type, lhs, field)
  elif node[0].typ.kind == tyTuple:
    debug node
  else:
    assert false, $(node[0].typ.kind)

proc gen_dot_expr(module: BModule; node: PNode): ValueRef =
  let adr = gen_dot_expr_lvalue(module, node)
  result = llvm.buildLoad(module.ll_builder, adr, "gen_dot_expr.deref")

proc gen_checked_field_lvalue(module: BModule; node: PNode): ValueRef =
  # todo: field checks
  result = gen_dot_expr_lvalue(module, node[0])

proc gen_checked_field_expr(module: BModule; node: PNode): ValueRef =
  let adr = gen_checked_field_lvalue(module, node)
  result = llvm.buildLoad(module.ll_builder, adr, "gen_checked_field_expr.deref")

# ------------------------------------------------------------------------------

proc gen_bracket_expr_lvalue(module: BModule; node: PNode): ValueRef =
  let lhs = gen_expr_lvalue(module, node[0])
  let rhs = gen_expr(module, node[1])
  assert lhs != nil
  assert rhs != nil
  #echo "lhs ", lhs
  #echo "rhs ", rhs
  case node[0].typ.kind:
  of tyArray:
    var indices = [constant_int(module, 0), rhs]
    result = llvm.buildGEP(module.ll_builder, lhs, addr indices[0], cuint len indices, "array.index")
  of tyUncheckedArray:
    assert false
  of tyOpenArray:
    let adr_field_data = build_field_ptr(module, lhs, constant(module, 0i32), "openarray.data")
    let adr_field_lengt = build_field_ptr(module, lhs, constant(module, 1i32), "openarray.length")
    let data_ptr = llvm.buildLoad(module.ll_builder, adr_field_data, "openarray.deref")
    var indices = [rhs]
    result = llvm.buildGEP(module.ll_builder, data_ptr, addr indices[0], cuint len indices, "openarray.index")
  of tyCString:
    var indices = [rhs]
    let adr = llvm.buildLoad(module.ll_builder, lhs, "cstring.deref")
    result = llvm.buildGEP(module.ll_builder, adr, addr indices[0], cuint len indices, "cstring.index")
  of tyString:
    assert false
  of tySequence:
    assert false
  of tyTuple:
    var indices = [constant_int(module, 0), rhs]
    echo "indices = ", indices
    echo "L = ", lhs
    echo "R = ", rhs
    assert module != nil
    assert module.ll_builder != nil
    assert lhs != nil
    assert rhs != nil
    assert addr(indices[0]) != nil
    result = llvm.buildGEP(module.ll_builder, lhs, addr indices[0], cuint len indices, "tuple.index")
  else:
    assert false

proc gen_bracket_expr(module: BModule; node: PNode): ValueRef =
  let adr = gen_bracket_expr_lvalue(module, node)
  result = llvm.buildLoad(module.ll_builder, adr, "gen_bracket_expr.deref")

# ------------------------------------------------------------------------------

proc gen_deref_expr_lvalue(module: BModule; node: PNode): ValueRef =
  # **value -> *value
  let adr = gen_expr_lvalue(module, node[0])
  result = llvm.buildLoad(module.ll_builder, adr, "deref.lvalue")

proc gen_deref_expr(module: BModule; node: PNode): ValueRef =
  # *value -> value
  let adr = gen_expr(module, node[0])
  result = llvm.buildLoad(module.ll_builder, adr, "deref.rvalue")

proc gen_addr(module: BModule; node: PNode): ValueRef =
  # addr foo
  result = gen_expr_lvalue(module, node[0])

proc gen_conv(module: BModule; node: PNode): ValueRef =
  let dst_type = node.typ
  let src_type = node[1].typ
  let value = gen_expr(module, node[1])
  let ll_src_type = llvm.typeOf(value)
  let ll_dst_type = get_type(module, dst_type)
  #echo "gen_conv:"
  #echo "dst_type = ", dst_type.kind, " -> ", ll_dst_type
  #echo "scr_type = ", src_type.kind, " -> ", ll_src_type
  #echo "value = ", value

  const Integers   = {tyInt .. tyInt64}
  const Floats     = {tyFloat .. tyFloat128}
  const Unsigned   = {tyUint .. tyUInt64}
  const Pointers   = {tyPtr, tyPointer, tyCString, tyRef}

  if ll_src_type == ll_dst_type:
    result = value

  elif (src_type.kind in Integers) and (dst_type.kind in Integers):
    # int -> int
    result = convert_scalar(module, value, ll_dst_type, true)

  elif (src_type.kind in Unsigned) and (dst_type.kind in Unsigned):
    # uint -> uint
    result = convert_scalar(module, value, ll_dst_type, false)

  elif (src_type.kind in {tyFloat32}) and (dst_type.kind in {tyFloat, tyFloat64}):
    # ext float
    result = llvm.buildFPExt(module.ll_builder, value, module.ll_float64, "")

  elif (src_type.kind in {tyFloat64, tyFloat}) and (dst_type.kind in {tyFloat32}):
    # trunc float
    result = llvm.buildFPTrunc(module.ll_builder, value, module.ll_float32, "")

  elif (src_type.kind in Floats) and (dst_type.kind in Integers):
    # float -> int
    result = llvm.buildFPToSI(module.ll_builder, value, ll_dst_type, "")

  elif (src_type.kind in Floats) and (dst_type.kind in Unsigned):
    # float -> uint
    result = llvm.buildFPToUI(module.ll_builder, value, ll_dst_type, "")

  elif (src_type.kind in Integers) and (dst_type.kind in Floats):
    # int -> float
    result = llvm.buildSIToFP(module.ll_builder, value, ll_dst_type, "")

  elif (src_type.kind in Unsigned) and (dst_type.kind in Floats):
    # uint -> float
    result = llvm.buildUIToFP(module.ll_builder, value, ll_dst_type, "")

  elif (src_type.kind in Pointers) and (dst_type.kind in Pointers):
    # pointer -> pointer
    result = llvm.buildBitCast(module.ll_builder, value, ll_dst_type, "")

  else:
    echo "unsupported conv", src_type.kind, " =====> ", dst_type.kind

proc gen_check_range_float(module: BModule; node: PNode): ValueRef =
  # todo
  discard

proc gen_check_range(module: BModule; node: PNode): ValueRef =
  # int6(v1)
  # node(node[0])
  # todo: range check
  let value = gen_expr(module, node[0])
  let dst_type = get_type(module, node.typ)
  let signed = is_signed_type(node[0].typ)
  result = convert_scalar(module, value, dst_type, signed)

proc gen_check_range64(module: BModule; node: PNode): ValueRef =
  # todo
  result = gen_check_range(module, node)

proc build_cast(module: BModule; value: ValueRef; dst_type: TypeRef): ValueRef =
  let src_type = llvm.typeOf(value)
  let src_kind = llvm.getTypeKind(src_type)
  let dst_kind = llvm.getTypeKind(dst_type)
  if   src_kind == PointerTypeKind and dst_kind == IntegerTypeKind:
    result = llvm.buildPtrToInt(module.ll_builder, value, dst_type, "cast.ptr_to_int")
  elif src_kind == IntegerTypeKind and dst_kind == PointerTypeKind:
    result = llvm.buildIntToPtr(module.ll_builder, value, dst_type, "cast.int_to_ptr")
  elif src_kind == PointerTypeKind and dst_kind == PointerTypeKind:
    result = llvm.buildPointerCast(module.ll_builder, value, dst_type, "cast.ptr_to_ptr")
  else:
    module.ice("v " & $value & " s " & $src_kind & " d " & $dst_kind)

proc gen_cast_expr_lvalue(module: BModule; node: PNode): ValueRef =
  let value = gen_expr_lvalue(module, node[1])
  let dst_type = type_to_ptr get_type(module, node[0].typ)
  result = build_cast(module, value, dst_type)

proc gen_cast_expr(module: BModule; node: PNode): ValueRef =
  let value = gen_expr(module, node[1])
  let dst_type = get_type(module, node[0].typ)
  result = build_cast(module, value, dst_type)

# ------------------------------------------------------------------------------

proc gen_expr_lvalue(module: BModule; node: PNode): ValueRef =
  # L value, always pointer
  #echo "gen_expr_lvalue kind: ", node.kind
  case node.kind:
  of nkSym:
    result = gen_sym_expr_lvalue(module, node)
  of nkDotExpr:
    result = gen_dot_expr_lvalue(module, node)
  of nkCheckedFieldExpr:
    result = gen_checked_field_lvalue(module, node)
  of nkBracketExpr:
    result = gen_bracket_expr_lvalue(module, node)
  of nkHiddenDeref, nkDerefExpr:
    result = gen_deref_expr_lvalue(module, node)
  of nkCast:
    result = gen_cast_expr_lvalue(module, node)
  else: echo "gen_expr_lvalue: unknown node kind: ", node.kind

  assert_value_type(result, PointerTypeKind)

proc gen_expr*(module: BModule; node: PNode): ValueRef =
  # R value, can be pointer or value
  #echo "gen_expr kind: ", node.kind
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
    result = gen_array_lit(module, node)
  of nkPar, nkTupleConstr:
    discard
  of nkObjConstr:
    result = gen_object_lit(module, node)
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
    discard
  of nkObjDownConv:
    discard
  of nkObjUpConv:
    discard
  of nkChckRangeF:
    result = gen_check_range_float(module, node)
  of nkChckRange64:
    result = gen_check_range64(module, node)
  of nkChckRange:
    result = gen_check_range(module, node)
  of nkStringToCString:
    discard
  of nkCStringToString:
    discard
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
    # todo: debug
    discard llvm.buildAlloca(module.ll_builder, module.ll_int32, node[0].strVal)
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
    discard
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
  else: echo "gen_expr: unknown node kind: ", node.kind

# ------------------------------------------------------------------------------

proc gen_stmt*(module: BModule; node: PNode) =
  let val = gen_expr(module, node)
