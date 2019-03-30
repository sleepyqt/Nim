import ast
from transf import transformBody
import llvm_data, llvm_type
import llvm_dll as llvm

proc gen_L_value*(module: BModule; node: PNode): ValueRef
proc gen_R_value*(module: BModule; node: PNode): ValueRef
proc gen_stmt_list*(module: BModule; node: PNode)

# ------------------------------------------------------------------------------

proc gen_int_lit(module: BModule; node: PNode): ValueRef =
  let ll_type = get_type(module, node.typ)
  assert(ll_type != nil)

proc gen_uint_lit(module: BModule; node: PNode): ValueRef =
  let ll_type = get_type(module, node.typ)
  assert(ll_type != nil)

proc gen_char_lit(module: BModule; node: PNode): ValueRef =
  let ll_type = get_type(module, node.typ)
  assert(ll_type != nil)

# ------------------------------------------------------------------------------

proc gen_sym_L_value(module: BModule; node: PNode): ValueRef =
  case node.sym.kind:
  of skVar, skLet:
    let value_ptr = module.get_value(node.sym.id)
    result = value_ptr
  else: echo "gen_sym_L_value: ", node.sym.kind

proc gen_sym_R_value(module: BModule; node: PNode): ValueRef =
  discard

# ------------------------------------------------------------------------------

proc gen_L_value(module: BModule; node: PNode): ValueRef =
  case node.kind:
  of nkSym: result = gen_sym_L_value(module, node)
  else: echo "get_L_value: unknown node kind: ", node.kind

proc gen_R_value(module: BModule; node: PNode): ValueRef =
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

# Procedure Types --------------------------------------------------------------

proc gen_proc(module: BModule; sym: PSym) =
  echo "[] gen_proc: ", sym.name.s
  let proc_type = getType(module, sym.typ)
  let m = printTypeToString(proc_type)
  echo "[", m, "]"
  disposeMessage(m)

# - Statements -----------------------------------------------------------------

proc gen_asgn(module: BModule; node: PNode) =
  echo "[] gen_asgn"
  let lhs = node[0]
  let rhs = node[1]
  assert lhs != nil
  assert rhs != nil
  let lhs_val = gen_L_value(module, lhs)
  let rhs_val = gen_R_value(module, rhs)

proc gen_single_var(module: BModule; node: PNode) =

  proc gen_global =
    discard

  proc gen_local =
    discard

  echo "[] gen_single_var"
  let name = node[0]
  let sym = node[0].sym
  if sfGlobal in sym.flags:
    gen_global()
  else:
    gen_local()

proc gen_var_section(module: BModule; node: PNode) =
  echo "[] gen_var_section"
  for def in node.sons:
    if def.kind == nkCommentStmt: continue
    if def.kind == nkIdentDefs:
      let id = def[0]
      let value = def[2]
      gen_single_var(module, def)

proc gen_if(module: BModule; node: PNode): ValueRef =
  echo "[] gen_if"

# ------------------------------------------------------------------------------

proc gen_sym_expr(module: BModule; node: PNode): ValueRef =
  case node.sym.kind:
  of skMethod:
    discard
  of skProc, skConverter, skIterator, skFunc:
    gen_proc(module, node.sym)
  of skConst:
    discard
  of skEnumField:
    discard
  of skVar, skForVar, skResult, skLet:
    discard
  of skTemp:
    discard
  of skParam:
    discard
  else: echo "gen_sym_expr: unknown symbol kind: ", node.sym.kind

proc gen_expr(module: BModule; node: PNode): ValueRef =
  case node.kind:
  of nkSym:
    result = gen_sym_expr(module, node)
  of nkNilLit:
    discard
  of nkStrLit..nkTripleStrLit:
    discard
  of nkIntLit..nkUInt64Lit, nkFloatLit..nkFloat128Lit, nkCharLit:
    discard
  of nkCall, nkHiddenCallConv, nkInfix, nkPrefix, nkPostfix, nkCommand, nkCallStrLit:
    discard
  of nkCurly:
    discard
  of nkBracket:
    discard
  of nkPar, nkTupleConstr:
    discard
  of nkObjConstr:
    discard
  of nkCast:
    discard
  of nkHiddenStdConv, nkHiddenSubConv, nkConv:
    discard
  of nkHiddenAddr, nkAddr:
    discard
  of nkBracketExpr:
    discard
  of nkDerefExpr, nkHiddenDeref:
    discard
  of nkDotExpr:
    discard
  of nkCheckedFieldExpr:
    discard
  of nkBlockExpr, nkBlockStmt:
    discard
  of nkStmtListExpr:
    discard
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
    discard
  of nkChckRange64:
    discard
  of nkChckRange:
    discard
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
    discard
  of nkVarSection, nkLetSection:
    gen_var_section(module, node)
  of nkConstSection:
    discard
  of nkForStmt:
    discard
  of nkCaseStmt:
    discard
  of nkReturnStmt:
    discard
  of nkBreakStmt:
    discard
  of nkAsgn:
    gen_asgn(module, node)
  of nkFastAsgn:
    discard
  of nkDiscardStmt:
    discard
  of nkAsmStmt:
    discard
  of nkTryStmt, nkHiddenTryStmt:
    discard
  of nkRaiseStmt:
    discard
  of nkTypeSection:
    discard
  of nkCommentStmt, nkIteratorDef, nkIncludeStmt, nkImportStmt, nkImportExceptStmt, nkExportStmt, nkExportExceptStmt, nkFromStmt, nkTemplateDef, nkMacroDef, nkStaticStmt:
    discard
  of nkPragma:
    discard
  of nkPragmaBlock:
    discard
  of nkProcDef, nkFuncDef, nkMethodDef, nkConverterDef:
    gen_proc(module, node.sym)
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

proc gen_stmt(module: BModule; node: PNode) =
  let val = gen_expr(module, node)

proc gen_stmt_list*(module: BModule; node: PNode) =
  echo "[] gen_stmt_list"
  for son in node.sons:
    gen_stmt(module, son)
