import ast
from transf import transformBody
import llvm_data, llvm_type
import llvm_dll as llvm

proc gen_L_value*(module: BModule; node: PNode): ValueRef
proc gen_R_value*(module: BModule; node: PNode): ValueRef
proc gen_stmt_list*(module: BModule; node: PNode)
proc gen_stmt*(module: BModule; node: PNode)

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

  let proc_name = sym.name.s & $sym.id
  let proc_val = llvm.addFunction(module.ll_module, proc_name, proc_type)

  module.open_scope()
  module.top_scope.proc_val = proc_val

  let entry_bb = llvm.appendBasicBlockInContext(
    module.ll_context,
    module.top_scope.proc_val,
    "entry")

  llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)

  let new_body = transformBody(module.module_list.graph, sym, cache = false)
  gen_stmt(module, new_body)

  discard llvm.buildRetVoid(module.ll_builder)

  module.close_scope()
  echo "[] end gen_proc: ", sym.name.s

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
  assert(module != nil)
  assert(node != nil)
  assert(module.top_scope != nil)
  assert(module.top_scope.proc_val != nil)

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

  let post_bb = llvm.appendBasicBlockInContext(
    module.ll_context,
    module.top_scope.proc_val,
    "post")

  echo "[] gen_if"
  for i in countup(0, sonsLen(node) - 1):
    let it = node.sons[i]
    if i == 0:
      # if
      discard
    if it.kind == nkElifBranch:
      # elif
      discard
    elif it.kind == nkElse:
      # else
      discard

# ------------------------------------------------------------------------------

proc gen_sym_expr(module: BModule; node: PNode): ValueRef =
  case node.sym.kind:
  of skMethod:
    discard
  of skProc, skConverter, skIterator, skFunc:
    #gen_proc(module, node.sym)
    discard
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
    if node.sons[genericParamsPos].kind == nkEmpty:
      gen_proc(module, node.sons[namePos].sym)
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

proc gen_stmt_list*(module: BModule; node: PNode) =
  echo "[] gen_stmt_list"
  for son in node.sons:
    echo "son ", son.kind
    gen_stmt(module, son)
