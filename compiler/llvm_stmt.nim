import ast
import llvm_data, llvm_expr, llvm_type, llvm_dll

proc gen_stmt*(module: BModule; node: PNode)

proc gen_stmt_list*(module: BModule; node: PNode) =
  for son in node.sons:
    gen_stmt(module, son)

# Variables --------------------------------------------------------------------

proc gen_local_var(module: BModule; node: PNode) =
  echo "× local var"
  let ll_val: ValueRef = nil
  module.add_value(node[0].sym.id, ll_val)

proc gen_global_var(module: BModule; node: PNode) =
  echo "× global var"
  let ll_val: ValueRef = nil
  module.add_value(node[0].sym.id, ll_val)

proc gen_ident_defs(module: BModule; node: PNode) =
  if node[0].kind == nkSym:
    if sfGlobal in node[0].sym.flags:
      gen_global_var(module, node)
    else:
      gen_local_var(module, node)

proc gen_asgn(module: BModule; node: PNode) =
  let lhs = node[0]
  let rhs = node[1]
  assert lhs != nil
  assert rhs != nil
  let lhs_val = gen_L_value(module, lhs)
  let rhs_val = gen_R_value(module, rhs)

# Control Flow -----------------------------------------------------------------

proc gen_if_stmt(module: BModule; node: PNode) =
  discard

proc gen_while_stmt(module: BModule; node: PNode) =
  discard

proc gen_for_stmt(module: BModule; node: PNode) =
  discard

# ------------------------------------------------------------------------------

proc gen_stmt*(module: BModule; node: PNode) =
  case node.kind:
  of nkStmtList, nkVarSection, nkLetSection:
    gen_stmt_list(module, node)
  #of nkProcDef: assert(false)
  of nkIdentDefs:
    gen_ident_defs(module, node)
  of nkAsgn:
    gen_asgn(module, node)
  else: echo "gen_stmt: Unknown node kind: ", node.kind
