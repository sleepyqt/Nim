# included from llvm_expr.nim

proc gen_asgn(module: BModule; node: PNode) =
  let lhs = gen_expr_lvalue(module, node[0])
  let rhs = gen_expr(module, node[1])
  assert lhs != nil, $node[0].kind
  assert rhs != nil, $node[1].kind
  gen_copy(module, lhs, rhs, node[0].typ)

proc build_global_var(module: BModule; sym: PSym; initializer: PNode): ValueRef =
  let ll_type = get_type(module, sym.typ)
  let name = mangle_global_var_name(module, sym)

  when spam:
    echo "☺☺ --------------------------------------------------"
    echo "☺☺ build_global_var : ", sym.name.s
    echo "☺☺ mangled name     : ", name
    echo "☺☺ id               : ", sym.id
    echo "☺☺ flags            : ", sym.flags
    echo "☺☺ --------------------------------------------------"

  result = llvm.addGlobal(module.ll_module, ll_type, name)
  llvm.setInitializer(result, llvm.constNull(ll_type))

  llvm.setVisibility(result, DefaultVisibility)

  if initializer == nil or initializer.kind == nkEmpty:
    gen_default_init(module, sym.typ, result)
  else:
    let value = gen_expr(module, initializer)
    assert value != nil, $initializer.kind
    assert result != nil
    gen_copy(module, result, value, initializer.typ)
  module.add_value(sym, result)

proc build_local_var(module: BModule; sym: PSym; initializer: PNode): ValueRef =
  let ll_type = get_type(module, sym.typ)
  let name = mangle_local_name(module, sym)

  result = build_entry_alloca(module, ll_type, name)

  if initializer == nil or initializer.kind == nkEmpty:
    gen_default_init(module, sym.typ, result)
  else:
    let value = gen_expr(module, initializer)
    assert value != nil, $initializer.kind
    assert result != nil
    gen_copy(module, result, value, initializer.typ)
  module.add_value(sym, result)

proc gen_var_prototype(module: BModule; node: PNode): ValueRef =
  let sym = node.sym
  let typ = node.sym.typ
  let ll_type = get_type(module, typ)
  let name = mangle_global_var_name(module, sym)
  let global = llvm.addGlobal(module.ll_module, ll_type, name)

  when spam:
    echo "%% --------------------------------------------------"
    echo "%% gen_var_prototype: ", sym.name.s
    echo "%% mangled name     : ", name
    echo "%% llvm type        : ", ll_type
    echo "%% --------------------------------------------------"

  llvm.setLinkage(global, ExternalLinkage)
  result = global

proc gen_closure_var(module: BModule; node: PNode) =
  debug node
  assert false

proc gen_tuple_var(module: BModule; node: PNode) =
  assert node.kind == nkVarTuple

  let length = sonsLen(node)

  for i in 0 .. (length - 3):
    if node[i].kind != nkSym:
      let lowered = lowerTupleUnpacking(module.module_list.graph, node, module.module_sym)
      discard gen_expr(module, lowered)
      return

  let tuple_var = gen_expr(module, node.lastSon)

  for i in 0 .. (length - 3):
    let i_node = node[i]
    let i_sym  = node[i].sym

    if sfCompileTime in i_sym.flags: continue

    let index  = constant(module, int32 i)
    let field_adr = build_field_ptr(module, tuple_var, index, "tuple.index")
    let field_val = llvm.buildLoad(module.ll_builder, field_adr, "tuple.index.value")

    if sfGlobal in i_sym.flags:
      let variable = build_global_var(module, i_sym, nil)
      gen_copy(module, variable, field_val, i_sym.typ)
    else:
      let variable = build_local_var(module, i_sym, nil)
      gen_copy(module, variable, field_val, i_sym.typ)

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

proc gen_if(module: BModule; node: PNode): ValueRef =
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
    result = build_entry_alloca(module, ll_type, "if.expr.result")

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
      let cond = build_i8_to_i1(module, gen_expr(module, it[0]))

      if else_bb == nil:
        # else branch missing
        discard llvm.buildCondBr(module.ll_builder, cond, then_bb, post_bb)
      else:
        discard llvm.buildCondBr(module.ll_builder, cond, then_bb, else_bb)

      llvm.positionBuilderAtEnd(module.ll_builder, then_bb)

      # *** emit then branch code ***

      let value = gen_expr(module, it.sons[1])
      if result != nil: gen_copy(module, result, value, node.typ)

      # terminate basic block if needed
      maybe_terminate(module, post_bb)

      else_bb = elif_bb
    elif it.kind in {nkElse, nkElseExpr}:
      # nkElse
      else_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "if.false")
      llvm.moveBasicBlockAfter(else_bb, entry_bb)
      llvm.positionBuilderAtEnd(module.ll_builder, else_bb)

      # *** emit else branch code ***

      let value = gen_expr(module, it.sons[0])
      if result != nil: gen_copy(module, result, value, node.typ)

      # terminate basic block if needed
      maybe_terminate(module, post_bb)

  llvm.positionBuilderAtEnd(module.ll_builder, post_bb)

  if result != nil and node.typ.kind notin {tyObject, tyTuple, tyArray}:
    result = llvm.buildLoad(module.ll_builder, result, "")

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
  let cond = build_i8_to_i1(module, gen_expr(module, node[0]))
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

  assert scope != nil

  if node[0].kind != nkEmpty:
    discard gen_expr(module, node[0])

  discard llvm.buildBr(module.ll_builder, scope.return_target)

proc gen_block(module: BModule; node: PNode): ValueRef =
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
  result = gen_expr(module, node[1])

  maybe_terminate(module, exit_bb)

  llvm.positionBuilderAtEnd(module.ll_builder, exit_bb)

  module.close_scope()

# ------------------------------------------------------------------------------

proc build_try_longjump(module: BModule; node: PNode) =
  module.open_scope()

  let entry_bb = llvm.getInsertBlock(module.ll_builder)
  let fun = llvm.getBasicBlockParent(entry_bb)
  let proc_scope = module.proc_scope()

  #llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)

  let try_block_value = gen_expr(module, node[0])

  for son in node:
    echo "> kind: ", son.kind
    if son.kind == nkExceptBranch:
      discard
    elif son.kind == nkFinally:
      discard
    else:
      discard

  module.close_scope()

proc build_raise_longjump(module: BModule; node: PNode) =
  discard

# ------------------------------------------------------------------------------

proc gen_try(module: BModule; node: PNode) =
  echo "> gen try stmt"
  #debug node
  #echo "end"

  case module.ehmodel:
  of EHModel.LongJump:
    build_try_longjump(module, node)
  of EHModel.WindowsSEH:
    assert false
  of EHModel.Itanium:
    assert false

  echo "> try end"

proc gen_raise(module: BModule; node: PNode) =
  echo "gen_raise:"
  debug node
  echo "end"
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

  llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)
  for i, branch in node.sons[1 .. ^1]:
    let branch_bb = basic_blocks[i]
    if branch.kind != nkElse:
      for cond in branch.sons[0 ..< ^1]:
        var cmp_result: ValueRef

        if cond.kind == nkRange:
          # range cond
          # lhs >= range_lo and lhs <= range_hi
          let range_lo = gen_expr(module, cond[0])
          let range_hi = gen_expr(module, cond[1])
          let c1 = build_generic_cmp(module, ">=", lhs, range_lo, node[0].typ)
          let c2 = build_generic_cmp(module, "<=", lhs, range_hi, node[0].typ)
          cmp_result = build_i8_to_i1(module, llvm.buildAnd(module.ll_builder, c1, c2, ""))
        else:
          # scalar cond
          let rhs =
            if cond.kind in {nkIntLit}: # literal may get wrong type...
              convert_scalar(module, gen_expr(module, cond), llvm.typeOf(lhs), is_signed_type(node[0].typ))
            else:
              gen_expr(module, cond)

          let cmp = build_generic_cmp(module, "==", lhs, rhs, node[0].typ)
          cmp_result = build_i8_to_i1(module, cmp)

        let save_bb = llvm.getInsertBlock(module.ll_builder)
        let cond_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "case.cond")
        llvm.moveBasicBlockAfter(cond_bb, save_bb)
        llvm.positionBuilderAtEnd(module.ll_builder, save_bb)
        discard llvm.buildCondBr(module.ll_builder, cmp_result, branch_bb, cond_bb)
        llvm.positionBuilderAtEnd(module.ll_builder, cond_bb)
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

proc gen_stmt_list_expr(module: BModule; node: PNode): ValueRef =
  for i in 0 .. node.len - 2:
    gen_stmt(module, node[i])
  result = gen_expr(module, node.lastSon)
