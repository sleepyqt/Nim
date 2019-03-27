import ast
import llvm_data, llvm_expr, llvm_type

proc gen_stmt*(module: BModule; node: PNode)

proc gen_stmt_list*(module: BModule; node: PNode) =
  for son in node.sons:
    gen_stmt(module, son)

# Variables --------------------------------------------------------------------

proc gen_ident_defs(module: BModule; node: PNode) =
  if node[0].kind == nkSym:
    if sfGlobal in node[0].sym.flags:
      echo "× global var"
    else:
      echo "× local var"

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
