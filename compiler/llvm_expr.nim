import ast
import llvm_data, llvm_dll, llvm_type

proc gen_L_value*(module: BModule; node: PNode): ValueRef
proc gen_R_value*(module: BModule; node: PNode): ValueRef

# ------------------------------------------------------------------------------

proc gen_int_lit(module: BModule; node: PNode): ValueRef =
  let ll_type = get_type(module, node.typ)

proc gen_uint_lit(module: BModule; node: PNode): ValueRef =
  let ll_type = get_type(module, node.typ)

proc gen_char_lit(module: BModule; node: PNode): ValueRef =
  let ll_type = get_type(module, node.typ)

# ------------------------------------------------------------------------------

proc gen_sym_L_value*(module: BModule; node: PNode): ValueRef =
  case node.sym.kind:
  of skVar, skLet:
    let value_ptr = module.get_value(node.sym.id)
    result = value_ptr
  else: echo "gen_sym_L_value: ", node.sym.kind

proc gen_sym_R_value*(module: BModule; node: PNode): ValueRef =
  discard

# ------------------------------------------------------------------------------

proc gen_L_value*(module: BModule; node: PNode): ValueRef =
  case node.kind:
  of nkSym: result = gen_sym_L_value(module, node)
  else: echo "get_L_value: unknown node kind: ", node.kind

proc gen_R_value*(module: BModule; node: PNode): ValueRef =
  case node.kind:
  of nkSym:
    result = gen_sym_R_value(module, node)
  of nkIntLit .. nkInt64Lit:
    result = gen_int_lit(module, node)
  of nkUintLit .. nkUInt64Lit :
    result = gen_uint_lit(module, node)
  of nkCharLit:
    result = gen_char_lit(module, node)
  else: echo "get_R_value: unknown node kind: ", node.kind
