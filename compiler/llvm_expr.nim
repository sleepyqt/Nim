import ast, types
from transf import transformBody
import llvm_data, llvm_type
import llvm_dll as llvm

proc gen_expr_lvalue*(module: BModule; node: PNode): ValueRef
proc gen_stmt*(module: BModule; node: PNode)
proc gen_expr(module: BModule; node: PNode): ValueRef

# ------------------------------------------------------------------------------

proc maybe_terminate(module: BModule; target: BasicBlockRef) =
  # try to terminate current block
  if llvm.getBasicBlockTerminator(getInsertBlock(module.ll_builder)) == nil:
    discard llvm.buildBr(module.ll_builder, target)

proc insert_entry_alloca(module: BModule; typ: TypeRef; name: cstring): ValueRef =
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

proc gen_default_init(module: BModule; typ: PType; alloca: ValueRef) =
  case typ.kind:
  of tyInt, tyUint:
    let val = llvm.constInt(module.ll_int, culonglong 0, Bool 0)
    discard llvm.buildStore(module.ll_builder, val, alloca)
  of tyInt8, tyUint8:
    let val = llvm.constInt(module.ll_int8, culonglong 0, Bool 0)
    discard llvm.buildStore(module.ll_builder, val, alloca)
  of tyInt16:
    let val = llvm.constInt(module.ll_int16, culonglong 0, Bool 0)
    discard llvm.buildStore(module.ll_builder, val, alloca)
  of tyInt32, tyUint32:
    let val = llvm.constInt(module.ll_int32, culonglong 0, Bool 0)
    discard llvm.buildStore(module.ll_builder, val, alloca)
  of tyInt64, tyUInt64:
    let val = llvm.constInt(module.ll_int64, culonglong 0, Bool 0)
    discard llvm.buildStore(module.ll_builder, val, alloca)
  of tyObject, tyArray:
    let adr = llvm.buildBitCast(module.ll_builder, alloca, module.ll_pointer, "")
    let size = getSize(module.module_list.config, typ)
    call_memset(module, adr, 0, int64 size)
  else: discard

# - Literals -------------------------------------------------------------------

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
  discard

proc gen_object_lit(module: BModule; node: PNode): ValueRef =
  let typ = get_type(module, node.typ)
  let alloca = insert_entry_alloca(module, typ, "object_literal")
  let adr = llvm.buildBitCast(module.ll_builder, alloca, module.ll_pointer, "")
  let size = getSize(module.module_list.config, node.typ)
  call_memset(module, adr, 0, size)
  result = alloca
  for i in 1 ..< node.len:
    let field = node[i]
    let index = get_field_index(module, node.typ, field[0].sym)
    echo "object field: ", i, " -> ", field.kind, " index: ", index, " sym: ", field[0].sym.kind

proc gen_array_lit(module: BModule; node: PNode): ValueRef =
  discard

proc gen_set_lit(module: BModule; node: PNode): ValueRef =
  discard

# ------------------------------------------------------------------------------

proc gen_sym_L_value(module: BModule; node: PNode): ValueRef =
  case node.sym.kind:
  of skVar, skLet:
    result = module.get_value(node.sym.id)
  else: echo "gen_sym_L_value: ", node.sym.kind

proc gen_sym_R_value(module: BModule; node: PNode): ValueRef =
  case node.sym.kind:
  of skVar, skLet:
    let alloca = gen_sym_L_value(module, node)
    result = llvm.buildLoad(module.ll_builder, alloca, "")
  else: echo "gen_sym_R_value: ", node.sym.kind

# ------------------------------------------------------------------------------

proc gen_magic_expr(module: BModule; node: PNode; op: TMagic): ValueRef =
  case op:
  of mAddI: discard
  of mSubI: discard
  of mMulI: discard
  of mDivI: discard
  of mModI: discard
  else: echo "unknown magic: ", op

proc gen_call_expr(module: BModule; node: PNode): ValueRef =
  discard

proc gen_call(module: BModule; node: PNode): ValueRef =
  if node[0].kind == nkSym and node[0].sym.magic != mNone:
    result = gen_magic_expr(module, node, node[0].sym.magic)
  else:
    result = gen_call_expr(module, node)

# ------------------------------------------------------------------------------

proc gen_expr_lvalue(module: BModule; node: PNode): ValueRef =
  # L value, always pointer
  case node.kind:
  of nkSym: result = gen_sym_L_value(module, node)
  else: echo "gen_expr_lvalue: unknown node kind: ", node.kind

# Procedure Types --------------------------------------------------------------

proc gen_proc(module: BModule; sym: PSym) =
  echo "******* generate proc: ", sym.name.s, " *******"

  # save current bb for nested procs
  let incoming_bb = llvm.getInsertBlock(module.ll_builder)

  let proc_type = getType(module, sym.typ)
  let m = printTypeToString(proc_type)
  echo "[", m, "]"
  disposeMessage(m)

  let proc_name = mangle_name(module, sym)
  let proc_val = llvm.addFunction(module.ll_module, proc_name, proc_type)

  module.open_scope()
  module.top_scope.proc_val = proc_val

  let entry_bb = llvm.appendBasicBlockInContext(
    module.ll_context,
    module.top_scope.proc_val,
    "entry")

  let return_bb = llvm.appendBasicBlockInContext(
    module.ll_context,
    module.top_scope.proc_val,
    "return")

  module.top_scope.return_target = return_bb

  llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)

  let new_body = transformBody(module.module_list.graph, sym, cache = false)
  gen_stmt(module, new_body)

  maybe_terminate(module, return_bb)
  llvm.moveBasicBlockAfter(return_bb, llvm.getInsertBlock(module.ll_builder))
  llvm.positionBuilderAtEnd(module.ll_builder, return_bb)

  discard llvm.buildRetVoid(module.ll_builder)

  module.close_scope()

  # viewFunctionCFG(proc_val)
  discard verifyFunction(proc_val, PrintMessageAction)

  llvm.positionBuilderAtEnd(module.ll_builder, incoming_bb)

  echo "******* end gen_proc: ", sym.name.s, " *******"

# - Statements -----------------------------------------------------------------

proc gen_asgn(module: BModule; node: PNode) =
  let lhs = node[0]
  let rhs = node[1]
  case lhs.typ.kind:
  of tyObject:
    let lhs_val = gen_expr_lvalue(module, lhs)
    let rhs_val = gen_expr(module, rhs)
    let dst = llvm.buildBitCast(module.ll_builder, lhs_val, module.ll_pointer, "")
    let src = llvm.buildBitCast(module.ll_builder, rhs_val, module.ll_pointer, "")
    let size = getSize(module.module_list.config, lhs.typ)
    call_memcpy(module, dst, src, size)
  of tyArray:
    discard
  of tyTuple:
    discard
  of tyInt .. tyUInt64:
    let lhs_val = gen_expr_lvalue(module, lhs)
    let rhs_val = gen_expr(module, rhs)
    discard llvm.buildStore(module.ll_builder, rhs_val, lhs_val)
  of tyString:
    discard
  of tyRef:
    discard
  of tySequence:
    discard
  else: assert(false)

proc gen_single_var(module: BModule; node: PNode) =

  proc gen_global =
    discard

  proc gen_local =
    let sym = node[0].sym
    let typ = node[0].sym.typ
    let ll_type = get_type(module, typ)
    let name = mangle_local_name(module, sym)
    let alloca = insert_entry_alloca(module, ll_type, name)
    # initialize variable
    if node[2].kind == nkEmpty:
      gen_default_init(module, typ, alloca)
    else:
      # initialize with exrp
      let val = gen_expr(module, node[2])
      assert(val != nil, "local val init nil")
      discard llvm.buildStore(module.ll_builder, val, alloca)
    echo "adding local: ", sym.id
    module.add_value(sym.id, alloca)

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

# - Control Flow ---------------------------------------------------------------

proc gen_if(module: BModule; node: PNode): ValueRef =
  # todo: if expression
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

  let entry_bb = llvm.getInsertBlock(module.ll_builder)
  let fun = llvm.getBasicBlockParent(entry_bb)

  let post_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "if.post")

  llvm.moveBasicBlockAfter(post_bb, entry_bb)

  var else_bb: BasicBlockRef = nil

  echo "[] gen_if"
  for i in countdown(sonsLen(node) - 1, 0):
    let it = node.sons[i]
    if it.kind == nkElifBranch:
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
      let cond_lhs = llvm.constInt(module.ll_bool, 0, 0) # todo: derp
      let cond_rhs = llvm.constInt(module.ll_bool, 0, 0)
      let cond = llvm.buildICmp(module.ll_builder, llvm.IntNE, cond_lhs, cond_rhs, "")
      if else_bb == nil:
        # else branch missing
        discard llvm.buildCondBr(module.ll_builder, cond, then_bb, post_bb)
      else:
        discard llvm.buildCondBr(module.ll_builder, cond, then_bb, else_bb)

      llvm.positionBuilderAtEnd(module.ll_builder, then_bb)

      # *** emit then branch code ***

      gen_stmt(module, it.sons[1])

      # terminate basic block if needed
      maybe_terminate(module, post_bb)

      else_bb = elif_bb
    elif it.kind == nkElse:
      # nkElse
      else_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "if.false")
      llvm.moveBasicBlockAfter(else_bb, entry_bb)
      llvm.positionBuilderAtEnd(module.ll_builder, else_bb)

      # *** emit else branch code ***

      gen_stmt(module, it.sons[0])

      # terminate basic block if needed
      maybe_terminate(module, post_bb)

  llvm.positionBuilderAtEnd(module.ll_builder, post_bb)

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
  let cond = llvm.constInt(module.ll_bool, 0, 0)
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
    discard gen_expr(module, node)

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

proc gen_stmt_list(module: BModule; node: PNode) =
  for son in node.sons:
    gen_stmt(module, son)

proc gen_stmt_list_expr(module: BModule; node: PNode): ValueRef =
  for i in 0 .. node.len - 2:
    gen_stmt(module, node[i])
  result = gen_expr(module, node.lastSon)

proc gen_expr(module: BModule; node: PNode): ValueRef =
  # R value, can be pointer or value
  echo "gen expr kind: ", node.kind
  case node.kind:
  of nkSym:
    result = gen_sym_expr(module, node)
  of nkNilLit:
    discard
  of nkStrLit .. nkTripleStrLit:
    discard
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
    gen_while(module, node)
  of nkVarSection, nkLetSection:
    gen_var_section(module, node)
  of nkConstSection:
    discard
  of nkForStmt:
    discard
  of nkCaseStmt:
    discard
  of nkReturnStmt:
    gen_return(module, node)
  of nkBreakStmt:
    gen_break(module, node)
  of nkAsgn:
    gen_asgn(module, node)
  of nkFastAsgn:
    discard
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
      if (sfBorrow notin sym.flags) and (sym.typ != nil):
        gen_proc(module, sym)
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
