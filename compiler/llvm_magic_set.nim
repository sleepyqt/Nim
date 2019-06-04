# included from llvm_expr.nim

proc build_set_mask(module: BModule; value: ValueRef; size: int): ValueRef =
  case size:
  of 1:
    let mask_const = constant(module, int8 7)
    result = llvm.buildAnd(module.ll_builder, value, mask_const, "")
  of 2:
    let mask_const = constant(module, int16 15)
    result = llvm.buildAnd(module.ll_builder, value, mask_const, "")
  of 4:
    let mask_const = constant(module, int32 31)
    result = llvm.buildAnd(module.ll_builder, value, mask_const, "")
  of 8:
    let mask_const = constant(module, int64 63)
    result = llvm.buildAnd(module.ll_builder, value, mask_const, "")
  else: assert false

proc gen_incl_set(module: BModule; node: PNode) =
  # "$1 |= ((NU8)1)<<(($2) & 7);$n"
  let set_type = node[1].typ
  let size = get_type_size(module, set_type)
  let dest = gen_expr_lvalue(module, node[1]).val # dest
  if size <= 8:
    let ll_type = get_type(module, set_type)
    let value = gen_expr(module, node[2]).val # v
    let one = llvm.constInt(ll_type, culonglong 1, Bool 0) # 1
    let mask = build_set_mask(module, value, int size) # v and 7
    let bit = llvm.buildShl(module.ll_builder, one, value, "set.incl.bit") # 1 << (v and 7)
    let dest_val = llvm.buildLoad(module.ll_builder, dest, "") # dest
    let new_val = llvm.buildOr(module.ll_builder, dest_val, bit, "") # dest or (1 << (v and 7))
    discard llvm.buildStore(module.ll_builder, new_val, dest)
  else:
    assert false

proc gen_excl_set(module: BModule; node: PNode) =
  # "$1 &= ~(((NU8)1) << (($2) & 7));$n"
  let set_type = node[1].typ
  let size = get_type_size(module, set_type)
  let dest = gen_expr_lvalue(module, node[1]).val # dest
  if size <= 8:
    let ll_type = get_type(module, set_type)
    let value = gen_expr(module, node[2]).val # v
    let one = llvm.constInt(ll_type, culonglong 1, Bool 0) # 1
    let mask = build_set_mask(module, value, int size) # v and 7
    let bit = llvm.buildShl(module.ll_builder, one, value, "set.excl.bit") # 1 << (v and 7)
    let bit_inv = llvm.buildNot(module.ll_builder, bit, "") # not (1 << (v and 7))
    let dest_val = llvm.buildLoad(module.ll_builder, dest, "") # dest
    let new_val = llvm.buildAnd(module.ll_builder, dest_val, bit_inv, "") # dest and (not (1 << (v and 7)))
    discard llvm.buildStore(module.ll_builder, new_val, dest)
  else:
    assert false

proc gen_card_set(module: BModule; node: PNode): ValueRef =
  # "#countBits32($1)"
  # "#countBits64($1)"
  assert false

proc gen_lt_set(module: BModule; node: PNode): ValueRef =
  # "(($1 & ~ $2 ==0)&&($1 != $2))"
  let set_type = node[1].typ
  let size = get_type_size(module, set_type)

  let lhs = gen_expr(module, node[1]).val
  let rhs = gen_expr(module, node[2]).val

  if size <= 8:
    let ll_type = get_type(module, set_type)
    let inv_rhs = llvm.buildNot(module.ll_builder, rhs, "") # not R
    let r0 = llvm.buildAnd(module.ll_builder, lhs, inv_rhs, "") # (L and (not R))
    let zero = constInt(ll_type, culonglong 0, Bool 0) # 0
    let c1 = llvm.buildICmp(module.ll_builder, IntEQ, r0, zero, "") # (L and (not R)) == 0
    let c2 = llvm.buildICmp(module.ll_builder, IntNE, lhs, rhs, "") # L != R
    result = llvm.buildAnd(module.ll_builder, c1, c2, "")
    result = build_i1_to_i8(module, result)
  else:
    assert false

proc gen_le_set(module: BModule; node: PNode): ValueRef =
  # "(($1 & ~ $2)==0)"
  let set_type = node[1].typ
  let size = get_type_size(module, set_type)

  let lhs = gen_expr(module, node[1]).val
  let rhs = gen_expr(module, node[2]).val

  if size <= 8:
    let ll_type = get_type(module, set_type)
    let inv_rhs = llvm.buildNot(module.ll_builder, rhs, "") # not R
    let r0 = llvm.buildAnd(module.ll_builder, lhs, inv_rhs, "") # (L and (not R))
    let zero = constInt(ll_type, culonglong 0, Bool 0) # 0
    result = llvm.buildICmp(module.ll_builder, IntEQ, r0, zero, "") # (L and (not R)) == 0
    result = build_i1_to_i8(module, result)
  else:
    assert false

proc gen_eq_set(module: BModule; node: PNode): ValueRef =
  # "($1 == $2)"
  let set_type = node[1].typ
  let size = get_type_size(module, set_type)

  let lhs = gen_expr(module, node[1]).val
  let rhs = gen_expr(module, node[2]).val

  if size <= 8:
    result = llvm.buildICmp(module.ll_builder, IntEq, lhs, rhs, "set.eq")
    result = build_i1_to_i8(module, result)
  else:
    assert false

proc gen_mul_set(module: BModule; node: PNode): ValueRef =
  # "($1 & $2)"
  let set_type = node[1].typ
  let size = get_type_size(module, set_type)

  let lhs = gen_expr(module, node[1]).val
  let rhs = gen_expr(module, node[2]).val

  if size <= 8:
    result = llvm.buildAnd(module.ll_builder, lhs, rhs, "set.mul")
  else:
    assert false

proc gen_plus_set(module: BModule; node: PNode): ValueRef =
  # "($1 | $2)"
  let set_type = node[1].typ
  let size = get_type_size(module, set_type)

  let lhs = gen_expr(module, node[1]).val
  let rhs = gen_expr(module, node[2]).val

  if size <= 8:
    result = llvm.buildOr(module.ll_builder, lhs, rhs, "set.plus")
  else:
    assert false

proc gen_minus_set(module: BModule; node: PNode): ValueRef =
  # "($1 & ~ $2)"
  let set_type = node[1].typ
  let size = get_type_size(module, set_type)

  let lhs = gen_expr(module, node[1]).val
  let rhs = gen_expr(module, node[2]).val

  if size <= 8:
    let inv = llvm.buildNot(module.ll_builder, rhs, "set.not")
    result = llvm.buildAnd(module.ll_builder, lhs, inv, "set.minus")
  else:
    assert false

proc gen_sym_diff_set(module: BModule; node: PNode): ValueRef =
  # "($1 ^ $2)"
  let set_type = node[1].typ
  let size = get_type_size(module, set_type)

  let lhs = gen_expr(module, node[1]).val
  let rhs = gen_expr(module, node[2]).val

  if size <= 8:
    result = llvm.buildXor(module.ll_builder, lhs, rhs, "set.sym_diff")
  else:
    assert false

proc gen_magic_in_set(module: BModule; node: PNode): ValueRef =
  let set_type = node[1].typ
  let set_size = get_type_size(module, set_type)
  let val_type = node[2].typ
  let ll_val_type = get_type(module, val_type)

  assert node[1] != nil
  assert node[2] != nil

  #echo "ll_val_type = ", ll_val_type

  if set_size <= 8:
    # result = if (set_value and (1 shl value)) != 0
    let set_value = gen_expr(module, node[1]).val
    let value = gen_expr(module, node[2]).val
    let one = llvm.constInt(ll_val_type, 1, Bool 0)
    let zero = llvm.constInt(ll_val_type, 0, Bool 0)
    let bit = llvm.buildShl(module.ll_builder, one, value, "inset.bit") # 1 shl value
    let merged = llvm.buildAnd(module.ll_builder, set_value, bit, "inset.merged") # set_value and (1 shl value)
    let r_bool =  llvm.buildICmp(module.ll_builder, IntNE, merged, zero, "inset.r_bool")
    result = build_i1_to_i8(module, r_bool)
  else:
    assert false
