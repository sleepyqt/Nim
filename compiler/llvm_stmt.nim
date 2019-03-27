import ast
import llvm_data

proc gen_stmt*(module: BModule; node: PNode)

proc gen_stmt_list*(module: BModule; node: PNode) =
  for son in node.sons:
    gen_stmt(module, son)

proc gen_stmt*(module: BModule; node: PNode) =
  case node.kind:
  of nkStmtList: gen_stmt_list(module, node)
  else: echo "gen_stmt: Unknown node kind: ", node.kind
