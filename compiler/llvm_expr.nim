import ast
import llvm_data, llvm_dll

proc gen_L_value*(module: BModule; node: PNode): ValueRef
proc gen_R_value*(module: BModule; node: PNode): ValueRef

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
  of nkSym: result = gen_sym_R_value(module, node)
  else: echo "get_R_value: unknown node kind: ", node.kind
