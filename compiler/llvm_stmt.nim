# included from "llvm_pass.nim"

proc gen_asgn(module: BModule; node: PNode) =
  let lhs = gen_expr_lvalue(module, node[0])
  let rhs = gen_expr(module, node[1])
  assert lhs.val != nil, $node[0].kind
  assert rhs.val != nil, $node[1].kind
  let typ = skipTypes(node[0].typ, abstractRange + tyUserTypeClasses + {tyStatic})

  build_assign(module, lhs, rhs, typ)

proc build_global_var(module: BModule; sym: PSym; initializer: PNode): BValue =
  let target = find_module(module, sym)
  result.val = target.get_value(sym)
  result.storage = OnHeap

  let typ = skipTypes(sym.typ, abstractInst)
  let ll_type = get_type(target, sym.typ)

  if result.val == nil: # else we already have variable declared somehow...
    let name = mangle_global_var_name(target, sym)
    result.val = llvm.addGlobal(target.ll_module, ll_type, name)
    target.add_value(sym, result.val)

    when spam_var:
      echo "☺☺ --------------------------------------------------"
      echo "☺☺ build_global_var : ", sym.name.s
      echo "☺☺ mangled name     : ", name
      echo "☺☺ id               : ", sym.id
      echo "☺☺ flags            : ", sym.flags
      echo "☺☺ loc flags        : ", sym.loc.flags
      echo "☺☺ --------------------------------------------------"

  if result.val != nil and llvm.getInitializer(result.val) == nil:
    # Its possible to use global before declaring it.
    # The 'use' will generate prototype, so check if
    # the var is prototype and 'upgrade' it to real variable.
    llvm.setInitializer(result.val, llvm.constNull(ll_type))
    llvm.setVisibility(result.val, DefaultVisibility)

    if sfThread in sym.flags:
      llvm.setThreadLocal(result.val, Bool 1)

    if initializer == nil or initializer.kind == nkEmpty:
      discard
    else:
      let value = gen_expr(target, initializer)
      assert value.val != nil, $initializer.kind
      assert result.val != nil
      #C gen_copy(target, result.val, value.val, skipTypes(initializer.typ, abstractInst))
      build_assign(module, result, value, skipTypes(initializer.typ, abstractInst))

proc build_local_var(module: BModule; sym: PSym; initializer: PNode): BValue =
  let typ = skipTypes(sym.typ, abstractInst)
  let ll_type = get_type(module, typ)
  let name = mangle_local_var_name(module, sym)

  result.val = build_entry_alloca(module, ll_type, name)
  result.storage = OnStack
  module.add_value(sym, result.val)

  if initializer == nil or initializer.kind == nkEmpty:
    gen_default_init(module, typ, result.val)
  else:
    let value = gen_expr(module, initializer)
    assert value.val != nil, $initializer.kind
    assert result.val != nil
    #gen_copy(module, result.val, value.val, initializer.typ)
    build_assign(module, result, value, skipTypes(initializer.typ, abstractInst))

proc gen_var_prototype(module: BModule; node: PNode): BValue =
  let sym = node.sym
  result.val = module.get_value(sym)
  if (result.val == nil):
    let typ = skipTypes(node.sym.typ, abstractInst)
    let ll_type = get_type(module, typ)
    let name = mangle_global_var_name(module, sym)
    result.val = llvm.addGlobal(module.ll_module, ll_type, name)
    module.add_value(sym, result.val)
    llvm.setLinkage(result.val, ExternalLinkage)

    if sfThread in sym.flags:
      llvm.setThreadLocal(result.val, Bool 1)

    when spam_var:
      echo "%% --------------------------------------------------"
      echo "%% gen_var_prototype: ", sym.name.s
      echo "%% mangled name     : ", name
      echo "%% id               : ", sym.id
      echo "%% llvm type        : ", ll_type
      echo "%% loc flags        : ", sym.loc.flags
      echo "%% owner id         : ", sym.owner.id
      echo "%% module id        : ", module.module_sym.id
      echo "%% --------------------------------------------------"

  assert result.val != nil
  result.storage = OnHeap

proc gen_closure_var(module: BModule; node: PNode) =
  debug node
  assert false

proc gen_tuple_var(module: BModule; node: PNode) =
  assert node.kind == nkVarTuple

  let length = sonsLen(node)

  for i in 0 .. (length - 3):
    if node[i].kind != nkSym:
      let lowered = lowerTupleUnpacking(module.module_list.graph, node, module.module_sym)
      discard gen_expr(module, lowered).val
      return

  let tuple_var = gen_expr(module, node.lastSon).val

  for i in 0 .. (length - 3):
    let i_node = node[i]
    let i_sym  = node[i].sym

    if sfCompileTime in i_sym.flags: continue

    let index  = constant(module, int32 i)
    let field_adr = build_field_ptr(module, tuple_var, index, "tuple.index")
    let field_val = llvm.buildLoad(module.ll_builder, field_adr, "tuple.index.value")

    if sfGlobal in i_sym.flags:
      let variable = build_global_var(module, i_sym, nil)
      gen_copy(module, variable.val, field_val, i_sym.typ)
    else:
      let variable = build_local_var(module, i_sym, nil)
      gen_copy(module, variable.val, field_val, i_sym.typ)

proc gen_single_var(module: BModule; node: PNode) =
  let sym = node[0].sym

  if (sfCompileTime notin sym.flags) and (lfNoDecl notin sym.loc.flags):
    if sfGlobal in sym.flags:
      discard build_global_var(module, sym, node[2])
    else:
      discard build_local_var(module, sym, node[2])

proc gen_var_section(module: BModule; node: PNode) =
  for def in node.sons:
    if def.kind == nkCommentStmt: continue
    if def.kind == nkIdentDefs:
      if def[0].kind == nkSym:
        let id = def[0]
        let value = def[2]
        gen_single_var(module, def)
      else:
        gen_closure_var(module, def)
    else:
      gen_tuple_var(module, def)

# - Control Flow ---------------------------------------------------------------

proc gen_if(module: BModule; node: PNode): BValue =
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
    result.val = build_entry_alloca(module, ll_type, "if.expr.result")

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
      let cond = build_i8_to_i1(module, gen_expr(module, it[0]).val)

      if else_bb == nil:
        # else branch missing
        discard llvm.buildCondBr(module.ll_builder, cond, then_bb, post_bb)
      else:
        discard llvm.buildCondBr(module.ll_builder, cond, then_bb, else_bb)

      llvm.positionBuilderAtEnd(module.ll_builder, then_bb)

      # *** emit then branch code ***

      let value = gen_expr(module, it.sons[1]).val
      if result.val != nil: gen_copy(module, result.val, value, node.typ)

      # terminate basic block if needed
      maybe_terminate(module, post_bb)

      else_bb = elif_bb
    elif it.kind in {nkElse, nkElseExpr}:
      # nkElse
      else_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "if.false")
      llvm.moveBasicBlockAfter(else_bb, entry_bb)
      llvm.positionBuilderAtEnd(module.ll_builder, else_bb)

      # *** emit else branch code ***

      let value = gen_expr(module, it.sons[0]).val
      if result.val != nil: gen_copy(module, result.val, value, node.typ)

      # terminate basic block if needed
      maybe_terminate(module, post_bb)

  llvm.positionBuilderAtEnd(module.ll_builder, post_bb)

  if result.val != nil and node.typ.kind notin {tyObject, tyTuple, tyArray}:
    result.val = llvm.buildLoad(module.ll_builder, result.val, "")

proc gen_while(module: BModule; node: PNode) =
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
  let cond = build_i8_to_i1(module, gen_expr(module, node[0]).val)
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

  let entry_bb = llvm.getInsertBlock(module.ll_builder)
  let fun = llvm.getBasicBlockParent(entry_bb)

  # code may appear after break statement!
  let dead_code = llvm.appendBasicBlockInContext(module.ll_context, fun, "break.dead_code")
  llvm.moveBasicBlockAfter(dead_code, entry_bb)
  llvm.positionBuilderAtEnd(module.ll_builder, dead_code)

proc gen_return(module: BModule; node: PNode) =
  let scope = module.proc_scope()

  assert scope != nil

  if node[0].kind != nkEmpty:
    discard gen_expr(module, node[0])

  discard llvm.buildBr(module.ll_builder, scope.return_target)

proc gen_block(module: BModule; node: PNode): BValue =
  module.open_scope()

  let block_name = if node[0].kind != nkEmpty: node[0].sym.name.s else: "block"

  let entry_bb = llvm.getInsertBlock(module.ll_builder)
  let fun = llvm.getBasicBlockParent(entry_bb)

  let exit_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "exit." & block_name)
  llvm.moveBasicBlockAfter(exit_bb, entry_bb)

  assert module.top_scope.break_target == nil

  module.top_scope.break_target = exit_bb
  if node[0].kind != nkEmpty:
    # named block
    module.top_scope.break_name = node[0].sym.id

  llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)
  result.val = gen_expr(module, node[1]).val

  maybe_terminate(module, exit_bb)

  llvm.positionBuilderAtEnd(module.ll_builder, exit_bb)

  module.close_scope()

proc gen_stmt_list_expr(module: BModule; node: PNode): BValue =
  for i in 0 .. node.len - 2:
    gen_stmt(module, node[i])
  result = gen_expr(module, node.lastSon)

# ------------------------------------------------------------------------------

proc build_try_longjump(module: BModule; node: PNode) =
  #[

  try:
    echo "1"
    echo "2"
  except ValueError:
    echo "v"
  except IOError, OSError:
    echo "i"
  finally:
    echo "f"

    safe_point: TSafePoint
    ....
  entry:
    pushSafePoint(safe_point)
    safe_point.status = setjmp(safe_point.context)
    br safe_point.status == 0 try.if.then try.if.else
  try.if.then:
    echo "1"
    echo "2"
    popSafePoint()
    br try.finally
  try.if.else:
    popSafePoint()                 #
    let %e = getCurrentException() #
    let %t0 = @ValueError          !!
    let %c0 = isObj(%e, %t0)       !
    br %c0 try.except0 try.next0   !
  try.except0:                     !
    safe_point.status = 0          !
    echo "v"                       !
    popCurrentException()          !
    br try.finally                 !
  try.next0:                       !
    let %t1 = @IOError             ++
    let %c1 = isObj(%e, %t1)       +
    br %c1 try.except1 try.next0.1 +
  try.next0.1:                     +
    let %t1.1 = @OSError           +
    let %c1.1 = isObj(%em %t1.1)   +
    br %c1.1 try.except1 try.next1 +
  try.except1:                     +
    safe_point.status = 0          +
    echo "i"                       +
    popCurrentException()          +
    br try.finally                 +
  try.next1:                       ++
    safe_point.status = 0
    echo "f"
    popCurrentException()
    br try.finally
  try.finally:
    echo "f"
    br safe_point.status != 0 try.reraise try.cont
  try.reraise:
    reraiseException()
    br try.cont
  try.cont
    ....
  ]#


  module.open_scope()

  let entry_bb = llvm.getInsertBlock(module.ll_builder)
  let fun = llvm.getBasicBlockParent(entry_bb)
  let proc_scope = module.proc_scope()

  let then_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "try.if.then")
  let else_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "try.if.else")
  let finally_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "try.finally")
  let reraise_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "try.reraise")
  let cont_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "try.cont")

  module.top_scope.unwind_target = else_bb

  llvm.moveBasicBlockAfter(then_bb, entry_bb)
  llvm.moveBasicBlockAfter(else_bb, then_bb)
  llvm.moveBasicBlockAfter(finally_bb, else_bb)
  llvm.moveBasicBlockAfter(reraise_bb, finally_bb)
  llvm.moveBasicBlockAfter(cont_bb, reraise_bb)

  # entry:
  let safe_point_type = get_runtime_type(module, "TSafePoint")
  let safe_point = build_entry_alloca(module, safe_point_type, "safe_point")
  let status_adr = build_field_ptr(module, safe_point, constant(module, int32 1)) # i64*
  let context_adr = build_field_ptr(module, safe_point, constant(module, int32 2)) # [N x i64]*
  discard gen_call_runtime_proc(module, "pushSafePoint", @[safe_point])

  var indices = [constant_int(module, 0), constant_int(module, 0)]
  let buff = llvm.buildBitCast(module.ll_builder, context_adr, module.ll_pointer, "try.context")

  let status = build_call_setjmp(module, buff)

  discard llvm.buildStore(
    module.ll_builder,
    llvm.buildSExt(module.ll_builder, status, module.ll_int, ""), status_adr)

  let cond1 = llvm.buildICmp(module.ll_builder,
    IntEQ,
    llvm.buildLoad(module.ll_builder, status_adr, "safe_point.status"),
    constant_int(module, 0),
    "")
  discard llvm.buildCondBr(module.ll_builder, cond1, then_bb, else_bb)

  # try.if.then:
  llvm.positionBuilderAtEnd(module.ll_builder, then_bb)
  let try_code = gen_expr(module, node[0]).val
  discard gen_call_runtime_proc(module, "popSafePoint", @[])
  discard llvm.buildBr(module.ll_builder, finally_bb)

  # try.if.else:
  # generate landing pad
  llvm.positionBuilderAtEnd(module.ll_builder, else_bb)
  discard gen_call_runtime_proc(module, "popSafePoint", @[])
  let exception = gen_call_runtime_proc(module, "getCurrentException", @[]) # Exception*

  var mtype_indices = [constant(module, int32 0), constant(module, int32 0), constant(module, int32 0)] # e.sup.m_type
  let exception_mtype_adr = llvm.buildGEP(module.ll_builder, exception, addr mtype_indices[0], cuint len mtype_indices, "")
  let exception_mtype = llvm.buildLoad(module.ll_builder, exception_mtype_adr, "try.exception.mtype")

  assert_value_type(exception, PointerTypeKind)

  # generate code for except branches
  assert module.top_scope.node == nil
  module.top_scope.node = node # set if we inside except branch

  for branch in node:
    if branch.kind == nkExceptBranch:
      if sonsLen(branch) == 1:
        # except branch without condition
        discard llvm.buildStore(module.ll_builder, constant_int(module, 0), status_adr)

        gen_stmt(module, branch[0])

        # path out return statement inside except brach
        let terminator = llvm.getBasicBlockTerminator(llvm.getInsertBlock(module.ll_builder))
        if terminator != nil:
          assert llvm.getInstructionOpcode(terminator) == Opcode.Br
          llvm.instructionEraseFromParent(terminator)

        discard gen_call_runtime_proc(module, "popCurrentException", @[])
      else:
        let incoming_bb = llvm.getInsertBlock(module.ll_builder)
        let except_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "try.except0")
        llvm.moveBasicBlockAfter(except_bb, incoming_bb)

        # try.except:
        llvm.positionBuilderAtEnd(module.ll_builder, except_bb)

        discard llvm.buildStore(module.ll_builder, constant_int(module, 0), status_adr)

        let except_code = gen_expr(module, lastSon(branch)).val

        let terminator = llvm.getBasicBlockTerminator(llvm.getInsertBlock(module.ll_builder))

        if terminator != nil:
          assert llvm.getInstructionOpcode(terminator) == Opcode.Br
          llvm.instructionEraseFromParent(terminator)

        discard gen_call_runtime_proc(module, "popCurrentException", @[])
        discard llvm.buildBr(module.ll_builder, finally_bb)

        # incoming:
        llvm.positionBuilderAtEnd(module.ll_builder, incoming_bb)

        for exc_type in branch.sons[0 ..< (sonsLen(branch) - 1)]:
          let next_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "try.next0")
          llvm.moveBasicBlockBefore(next_bb, except_bb)

          let type_info = gen_type_info(module, exc_type.typ)
          let is_obj = gen_call_runtime_proc(module, "isObj", @[exception_mtype, type_info])
          let cond = build_i8_to_i1(module, is_obj)
          discard llvm.buildCondBr(module.ll_builder, cond, except_bb, next_bb)

          llvm.positionBuilderAtEnd(module.ll_builder, next_bb)


  discard llvm.buildBr(module.ll_builder, finally_bb)

  assert module.top_scope.node != nil
  module.top_scope.node = nil

  # generate code for finally branch

  # try.finally
  llvm.positionBuilderAtEnd(module.ll_builder, finally_bb)

  for branch in node:
    if branch.kind == nkFinally:
      let finally_branch_code = gen_expr(module, branch[0]).val

  let cond2 = llvm.buildICmp(module.ll_builder,
    IntNE,
    llvm.buildLoad(module.ll_builder, status_adr, "safe_point.status"),
    constant_int(module, 0),
    "")
  discard llvm.buildCondBr(module.ll_builder, cond2, reraise_bb, cont_bb)

  # try.reraise
  llvm.positionBuilderAtEnd(module.ll_builder, reraise_bb)
  discard gen_call_runtime_proc(module, "reraiseException", @[])
  discard llvm.buildBr(module.ll_builder, cont_bb)

  # try.cont
  llvm.positionBuilderAtEnd(module.ll_builder, cont_bb)

  module.close_scope()

proc build_raise_longjump(module: BModule; node: PNode) =
  let try_scope = module.try_scope()

  if try_scope != nil and try_scope.node != nil:
    assert try_scope.node.kind in {nkTryStmt, nkHiddenTryStmt}
    # run cleanups if exception raised inside except brach
    for branch in try_scope.node:
      if branch.kind == nkFinally:
        let finally_branch_code = gen_expr(module, branch[0]).val

  if node[0].kind == nkEmpty:
    discard gen_call_runtime_proc(module, "reraiseException", @[])
  else:
    let typ = skipTypes(node[0].typ, abstractPtrs)
    let cast_ty = type_to_ptr get_runtime_type(module, "Exception")
    let exception = gen_expr(module, node[0]).val
    let cast_exception = llvm.buildBitCast(module.ll_builder, exception, cast_ty, "")
    assert typ.sym.name.s != ""
    let name = build_cstring_lit(module, typ.sym.name.s).val
    discard gen_call_runtime_proc(module, "raiseException", @[cast_exception, name])

# ------------------------------------------------------------------------------

proc gen_try(module: BModule; node: PNode) =
  case module.ehmodel:
  of EHModel.LongJump:
    build_try_longjump(module, node)
  of EHModel.WindowsSEH:
    assert false
  of EHModel.Itanium:
    assert false

proc gen_raise(module: BModule; node: PNode) =
  case module.ehmodel:
  of EHModel.LongJump:
    build_raise_longjump(module, node)
  of EHModel.WindowsSEH:
    assert false
  of EHModel.Itanium:
    assert false

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

  let lhs = gen_expr(module, node[0]).val

  var basic_blocks: seq[BasicBlockRef]

  for branch in node.sons[1 .. ^1]:
    let name = if branch.kind == nkOfBranch: "case.branch" else: "case.else"
    let branch_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, name)
    llvm.positionBuilderAtEnd(module.ll_builder, branch_bb)
    # generate branch code
    let val = gen_expr(module, branch.lastSon).val
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
          let range_lo = gen_expr(module, cond[0]).val
          let range_hi = gen_expr(module, cond[1]).val
          let c1 = build_generic_cmp(module, ">=", lhs, range_lo, node[0].typ)
          let c2 = build_generic_cmp(module, "<=", lhs, range_hi, node[0].typ)
          cmp_result = build_i8_to_i1(module, llvm.buildAnd(module.ll_builder, c1, c2, ""))
        else:
          # scalar cond
          let rhs =
            if cond.kind in {nkIntLit}: # literal may get wrong type...
              convert_scalar(module, gen_expr(module, cond).val, llvm.typeOf(lhs), is_signed_type(node[0].typ))
            else:
              gen_expr(module, cond).val

          let cmp = build_generic_cmp(module, "==", lhs, rhs, node[0].typ)
          cmp_result = build_i8_to_i1(module, cmp)

        let save_bb = llvm.getInsertBlock(module.ll_builder)
        let cond_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "case.cond")
        llvm.moveBasicBlockAfter(cond_bb, save_bb)
        llvm.positionBuilderAtEnd(module.ll_builder, save_bb)
        discard llvm.buildCondBr(module.ll_builder, cmp_result, branch_bb, cond_bb)
        llvm.positionBuilderAtEnd(module.ll_builder, cond_bb)
        if branch == node.sons[^1]:
          discard llvm.buildBr(module.ll_builder, exit_bb)
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
