import ast, types
from transf import transformBody
import llvm_data, llvm_type
import llvm_dll as llvm

proc gen_expr_lvalue*(module: BModule; node: PNode): ValueRef
proc gen_stmt*(module: BModule; node: PNode)
proc gen_expr(module: BModule; node: PNode): ValueRef
proc gen_proc(module: BModule; sym: PSym): ValueRef

# ------------------------------------------------------------------------------

proc `$`(x: ValueRef): string =
  if x == nil:
    result = "nil ValueRef"
  else:
    let str = llvm.printValueToString(x)
    result = $str
    disposeMessage(str)

proc `$`(x: TypeRef): string =
  if x == nil:
    result = "nil TypeRef"
  else:
    let str = llvm.printTypeToString(x)
    result = $str
    disposeMessage(str)

template ensure_type_kind(val: ValueRef; kind: TypeKind) =
  when true:
    assert val != nil
    let typ = llvm.typeOf(val)
    if llvm.getTypeKind(typ) != kind:
      let type_name = llvm.printTypeToString(typ)
      let value_name = llvm.printValueToString(val)
      echo "#### ensure_type_kind fail:"
      echo "#### value = ", value_name
      echo "#### type  = ", type_name
      disposeMessage(type_name)
      disposeMessage(value_name)
      #assert false

proc gen_copy(module: BModule; lhs, rhs: ValueRef; typ: PType) =
  assert lhs != nil
  assert rhs != nil
  assert typ != nil
  echo "gen_copy typ: ", typ.kind
  case typ.kind:
  of tyObject, tyArray, tyTuple:
    ensure_type_kind(lhs, PointerTypeKind)
    ensure_type_kind(rhs, PointerTypeKind)
    let dst = llvm.buildBitCast(module.ll_builder, lhs, module.ll_pointer, "")
    let src = llvm.buildBitCast(module.ll_builder, rhs, module.ll_pointer, "")
    let size = getSize(module.module_list.config, typ)
    call_memcpy(module, dst, src, size)
  of tyInt .. tyInt64, tyFloat .. tyFloat128, tyUInt .. tyUInt64:
    discard llvm.buildStore(module.ll_builder, rhs, lhs)
  of tyPtr, tyPointer, tyCString:
    discard llvm.buildStore(module.ll_builder, rhs, lhs)
  of tyBool, tyChar, tyEnum:
    discard llvm.buildStore(module.ll_builder, rhs, lhs)
  of tyString:
    discard
  of tyRef:
    discard
  of tySequence:
    discard
  else: assert(false)

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
  assert typ != nil
  assert alloca != nil
  case typ.kind:
  of tyInt, tyUint:
    let val = llvm.constInt(module.ll_int, culonglong 0, Bool 0)
    discard llvm.buildStore(module.ll_builder, val, alloca)
  of tyInt8, tyUint8, tyBool:
    let val = llvm.constInt(module.ll_int8, culonglong 0, Bool 0)
    discard llvm.buildStore(module.ll_builder, val, alloca)
  of tyInt16, tyUint16:
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

proc i8_to_i1(module: BModule; value: ValueRef): ValueRef =
  if llvm.getValueKind(value) == InstructionValueKind:
    # eleminate common zext trunc combo
    if llvm.getInstructionOpcode(value) == llvm.ZExt:
      result = llvm.getOperand(value, 0)
      if llvm.getFirstUse(value) == nil:
        llvm.instructionEraseFromParent(value)
      return
  result = llvm.buildTrunc(module.ll_builder, value, module.ll_logic_bool, "")

proc i1_to_i8(module: BModule; value: ValueRef): ValueRef =
  result = llvm.buildZExt(module.ll_builder, value, module.ll_bool, "")

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
    var indices = [
      llvm.constInt(module.ll_int32, culonglong 0, Bool 0),
      llvm.constInt(module.ll_int32, culonglong index, Bool 0)]
    let lhs = llvm.buildGEP(module.ll_builder, alloca, addr indices[0], cuint len indices, "")
    let rhs = gen_expr(module, field[1])
    gen_copy(module, lhs, rhs, field[0].typ)

proc gen_array_lit(module: BModule; node: PNode): ValueRef =
  discard

proc gen_set_lit(module: BModule; node: PNode): ValueRef =
  discard

# ------------------------------------------------------------------------------

proc gen_logic_and(module: BModule; node: PNode): ValueRef =
  discard

proc gen_logic_or(module: BModule; node: PNode): ValueRef =
  let incoming_bb = llvm.getInsertBlock(module.ll_builder)
  let fun = llvm.getBasicBlockParent(incoming_bb)
  let end_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "or.end")
  var rhs_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "or.rhs")
  llvm.moveBasicBlockAfter(rhs_bb, incoming_bb)
  llvm.moveBasicBlockAfter(end_bb, rhs_bb)

  # evaluate lhs first
  llvm.positionBuilderAtEnd(module.ll_builder, incoming_bb)
  let lhs = i8_to_i1(module, gen_expr(module, node[1]))
  # reacquire basic block, as new nodes may have inserted
  let lhs_bb = llvm.getInsertBlock(module.ll_builder)
  # short-circuit if lhs is true
  discard llvm.buildCondBr(module.ll_builder, lhs, end_bb, rhs_bb)

  # evaluate rhs
  llvm.positionBuilderAtEnd(module.ll_builder, rhs_bb)
  let rhs = i8_to_i1(module, gen_expr(module, node[2]))
  # reacquire rhs basic block
  rhs_bb = llvm.getInsertBlock(module.ll_builder)
  discard llvm.buildBr(module.ll_builder, end_bb)

  # phi node
  llvm.positionBuilderAtEnd(module.ll_builder, end_bb)
  let phi = llvm.buildPHI(module.ll_builder, module.ll_logic_bool, "")
  var values = [llvm.constInt(module.ll_logic_bool, culonglong 1, Bool 0), rhs]
  var blocks = [lhs_bb, rhs_bb]
  llvm.addIncoming(phi, addr values[0], addr blocks[0], 2)
  result = i1_to_i8(module, phi)

proc gen_magic_echo(module: BModule; node: PNode) =
  #for i in 1 ..< node.len:
  #  let arg_node = node[i]
  discard

proc gen_magic_expr(module: BModule; node: PNode; op: TMagic): ValueRef =

  proc binary(prc: BinaryProc): ValueRef =
    let lhs = gen_expr(module, node[1])
    let rhs = gen_expr(module, node[2])
    assert lhs != nil
    assert rhs != nil
    result = prc(module.ll_builder, lhs, rhs, "")

  proc icmp(pred: IntPredicate): ValueRef =
    let lhs = gen_expr(module, node[1])
    let rhs = gen_expr(module, node[2])
    result = llvm.buildICmp(module.ll_builder, pred, lhs, rhs, "")
    result = i1_to_i8(module, result)

  case op:
  # signed int
  of mAddI: result = binary(llvm.buildAdd)
  of mSubI: result = binary(llvm.buildSub)
  of mMulI: result = binary(llvm.buildMul)
  of mDivI: result = binary(llvm.buildSDiv)
  of mModI: result = binary(llvm.buildSRem)
  # unsigned int
  of mAddU: result = binary(llvm.buildAdd)
  of mSubU: result = binary(llvm.buildSub)
  of mMulU: result = binary(llvm.buildMul)
  of mDivU: result = binary(llvm.buildUDiv)
  of mModU: result = binary(llvm.buildURem)
  # integer cmp
  of mEqI: result = icmp(llvm.IntEQ) # todo: unsigned
  of mLeI: result = icmp(llvm.IntSLE)
  of mLtI: result = icmp(llvm.IntSLT)
  # boolean
  of mOr: result = gen_logic_or(module, node)
  of mAnd: result = gen_logic_and(module, node)
  # bitwise
  of mShrI: discard
  of mShlI: discard
  of mAshrI: discard
  of mBitandI: result = binary(llvm.buildAnd)
  of mBitorI: result = binary(llvm.buildOr)
  of mBitxorI: result = binary(llvm.buildXor)
  # float 32
  # float 64
  # misc
  of mEcho: gen_magic_echo(module, node)
  else: echo "unknown magic: ", op

# Procedure Types --------------------------------------------------------------

proc gen_call_expr(module: BModule; node: PNode): ValueRef =
  echo ""
  echo "*** gen_call_expr ***"
  var args: seq[ValueRef]
  let proc_sym   = node[0].sym
  let ret_type   = getReturnType(proc_sym)
  let callee_val = gen_expr(module, node[0])

  if ret_type != nil and ret_type.kind in CompositeTypes:
    result = insert_entry_alloca(module, get_type(module, ret_type), "call.tmp")
    args.add(result)

  # todo copy composite

  for i in 1 ..< node.len:
    echo "× arg: ", node[i].kind
    let arg_node = node[i]
    let arg_type = node[i].typ
    let arg_value = gen_expr(module, arg_node)
    if arg_type.kind in CompositeTypes:
      args.add arg_value
    elif arg_type.kind == tyBool:
      args.add llvm.buildTrunc(module.ll_builder, arg_value, module.ll_logic_bool, "")
    else:
      args.add arg_value

  let args_addr = if args.len == 0: nil else: addr args[0]
  let call_result = llvm.buildCall(module.ll_builder, callee_val, args_addr, cuint len args, "")

  if ret_type != nil and ret_type.kind notin CompositeTypes:
    result = call_result

proc gen_call(module: BModule; node: PNode): ValueRef =
  if node[0].kind == nkSym and node[0].sym.magic != mNone:
    result = gen_magic_expr(module, node, node[0].sym.magic)
  else:
    result = gen_call_expr(module, node)

proc gen_importc_proc(module: BModule; sym: PSym): ValueRef =
  let proc_type = getType(module, sym.typ)
  let proc_name = sym.name.s
  let proc_val = llvm.addFunction(module.ll_module, proc_name, proc_type)
  module.add_value(sym.id, proc_val)

proc gen_proc(module: BModule; sym: PSym): ValueRef =
  assert sym != nil
  if (sfBorrow in sym.flags) or (sym.typ == nil):
    return
  result = module.get_value(sym.id)
  if result == nil:
    echo "******* generate proc: ", sym.name.s, " *******"

    if sfImportc in sym.flags:
      return gen_importc_proc(module, sym)

    # save current bb for nested procs
    let incoming_bb = llvm.getInsertBlock(module.ll_builder)

    let proc_type  = getType(module, sym.typ)
    let proc_name  = mangle_name(module, sym)
    let proc_val   = llvm.addFunction(module.ll_module, proc_name, proc_type)
    let ret_type   = getReturnType(sym)
    let entry_bb   = llvm.appendBasicBlockInContext(module.ll_context, proc_val, "entry")
    let return_bb  = llvm.appendBasicBlockInContext(module.ll_context, proc_val, "return")
    var ret_addr: ValueRef # addres of *result* variable

    module.add_value(sym.id, proc_val)

    module.open_scope()
    module.top_scope.proc_val = proc_val
    module.top_scope.return_target = return_bb

    llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)

    # generate formal parameters and implicit *result* variable

    var param_index = 0
    if ret_type != nil and ret_type.kind in CompositeTypes:
      # result is composite type
      param_index = 1 # skip *result* param
      ret_addr = llvm.getParam(proc_val, cuint 0)
      llvm.setValueName2(ret_addr, "result", csize len "result")
      module.add_value(sym.ast.sons[resultPos].sym.id, ret_addr)
      llvm.addAttributeAtIndex(proc_val, AttributeIndex 1, module.ll_sret)
    elif ret_type != nil and ret_type.kind notin CompositeTypes:
      # result is scalar
      ret_addr = insert_entry_alloca(module, get_type(module, ret_type), "proc.result")
      module.add_value(sym.ast.sons[resultPos].sym.id, ret_addr)
      if ret_type.kind == tyBool:
        llvm.addAttributeAtIndex(proc_val, AttributeIndex 0, module.ll_zeroext)
    else:
      # result is void
      discard

    for index, param in sym.typ.n.sons[1 .. ^1]:
      if isCompileTimeOnly(param.sym.typ): continue
      let param_value = llvm.getParam(proc_val, cuint param_index)
      let param_type  = llvm.typeOf(param_value)
      let param_name  = param.sym.name.s
      let first_arg_offset = if (ret_type != nil) and (ret_type.kind in CompositeTypes): 2 else: 1
      assert param_value != nil
      assert param_type != nil
      echo "<><><> generate param: ", param_name, " ", index
      llvm.setValueName2(param_value, param_name, csize len param_name)
      if param.sym.typ.kind in CompositeTypes:
        # copied by caller
        module.add_value(param.sym.id, param_value)
        llvm.addAttributeAtIndex(proc_val, AttributeIndex index + first_arg_offset, module.ll_byval)
      elif param.sym.typ.kind == tyBool:
        # zero-extend bool params
        let param_addr = insert_entry_alloca(module, module.ll_bool, param_name & ".param")
        let param_i8 = llvm.buildZExt(module.ll_builder, param_value, module.ll_bool, "")
        discard llvm.buildStore(module.ll_builder, param_i8, param_addr)
        module.add_value(param.sym.id, param_addr)
        llvm.addAttributeAtIndex(proc_val, AttributeIndex index + first_arg_offset, module.ll_zeroext)
      else:
        # copy to local
        let param_addr = insert_entry_alloca(module, param_type, param_name & ".param")
        discard llvm.buildStore(module.ll_builder, param_value, param_addr)
        module.add_value(param.sym.id, param_addr)
      inc param_index

    # generate body
    let new_body = transformBody(module.module_list.graph, sym, cache = false)
    gen_stmt(module, new_body)

    maybe_terminate(module, return_bb)
    llvm.moveBasicBlockAfter(return_bb, llvm.getInsertBlock(module.ll_builder))
    llvm.positionBuilderAtEnd(module.ll_builder, return_bb)

    # generate ret
    if ret_type != nil and ret_type.kind in CompositeTypes:
      # result is composite type
      discard llvm.buildRetVoid(module.ll_builder)
    elif ret_type != nil and ret_type.kind == tyBool:
      # result is bool
      let ret_value = llvm.buildLoad(module.ll_builder, ret_addr, "")
      discard llvm.buildRet(
        module.ll_builder,
        llvm.buildTrunc(module.ll_builder, ret_value, module.ll_logic_bool, ""))
    elif ret_type != nil and ret_type.kind notin CompositeTypes:
      # result is scalar
      let ret_value = llvm.buildLoad(module.ll_builder, ret_addr, "")
      discard llvm.buildRet(module.ll_builder, ret_value)
    else:
      # result is void
      discard llvm.buildRetVoid(module.ll_builder)

    module.close_scope()

    llvm.positionBuilderAtEnd(module.ll_builder, incoming_bb)

    result = proc_val

    echo "******* verifying proc ***********************"
    #viewFunctionCFG(proc_val)
    discard verifyFunction(proc_val, PrintMessageAction)
    echo "******* end gen_proc: ", sym.name.s, " *******"

# - Statements -----------------------------------------------------------------

proc gen_asgn(module: BModule; node: PNode) =
  let lhs = gen_expr_lvalue(module, node[0])
  let rhs = gen_expr(module, node[1])
  assert lhs != nil
  assert rhs != nil
  gen_copy(module, lhs, rhs, node[0].typ)

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
      assert val != nil
      assert alloca != nil
      gen_copy(module, alloca, val, node[2].typ)
      #discard llvm.buildStore(module.ll_builder, val, alloca)
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
      let cond = i8_to_i1(module, gen_expr(module, it[0]))

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
  let cond = i8_to_i1(module, gen_expr(module, node[0]))
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
    discard gen_expr(module, node[0])

proc gen_stmt_list(module: BModule; node: PNode) =
  for son in node.sons:
    gen_stmt(module, son)

# ------------------------------------------------------------------------------

proc gen_stmt_list_expr(module: BModule; node: PNode): ValueRef =
  for i in 0 .. node.len - 2:
    gen_stmt(module, node[i])
  result = gen_expr(module, node.lastSon)

# ------------------------------------------------------------------------------

proc gen_sym_expr_lvalue(module: BModule; node: PNode): ValueRef =
  echo "gen_sym_expr_lvalue kind: ", node.sym.kind
  case node.sym.kind:
  of skVar, skForVar, skLet, skResult, skParam:
    result = module.get_value(node.sym.id)
  else: echo "gen_sym_expr_lvalue: ", node.sym.kind

  assert result != nil
  ensure_type_kind(result, PointerTypeKind)

proc gen_sym_expr(module: BModule; node: PNode): ValueRef =
  case node.sym.kind:
  of skMethod:
    discard
  of skProc, skConverter, skIterator, skFunc:
    if sfCompileTime in node.sym.flags:
      assert(false)
    result = gen_proc(module, node.sym)
  of skConst:
    discard
  of skEnumField:
    discard
  of skVar, skForVar, skResult, skLet, skParam:
    let alloca = gen_sym_expr_lvalue(module, node)
    case node.sym.typ.kind:
    of tyObject, tyArray, tyTuple:
      result = alloca
    else:
      result = llvm.buildLoad(module.ll_builder, alloca, "")
  of skTemp:
    discard
  else: echo "gen_sym_expr: unknown symbol kind: ", node.sym.kind

# ------------------------------------------------------------------------------

proc gen_dot_expr_lvalue(module: BModule; node: PNode): ValueRef =
  echo "gen_dot_expr_lvalue:"
  echo "node.kind    = ", node.kind
  echo "node[0].kind = ", node[0].kind
  echo "node[1].kind = ", node[1].kind
  echo "node[0].typ.kind = ", node[0].typ.kind
  echo "node[1].typ.kind = ", node[1].typ.kind
  #assert node[0].kind == nkSym
  #assert node[1].kind == nkSym
  let lhs = gen_expr_lvalue(module, node[0])
  let index = get_field_index(module, node[0].typ, node[1].sym)
  var indices = [
      llvm.constInt(module.ll_int32, culonglong 0, Bool 0),
      llvm.constInt(module.ll_int32, culonglong index, Bool 0)]
  result = llvm.buildGEP(module.ll_builder, lhs, addr indices[0], cuint len indices, "")
  echo "result = ", result, " ", llvm.typeOf(result)

proc gen_dot_expr(module: BModule; node: PNode): ValueRef =
  let adr = gen_dot_expr_lvalue(module, node)
  result = llvm.buildLoad(module.ll_builder, adr, "")

# ------------------------------------------------------------------------------

proc gen_bracket_expr_lvalue(module: BModule; node: PNode): ValueRef =
  echo "gen_bracket_expr_lvalue:"
  echo "node.kind = ", node.kind
  let lhs = gen_expr_lvalue(module, node[0])
  let rhs = gen_expr(module, node[1])
  echo "lhs = ", lhs
  echo "rhs = ", rhs
  var indices = [
      llvm.constInt(module.ll_int32, culonglong 0, Bool 0),
      rhs]
  result = llvm.buildGEP(module.ll_builder, lhs, addr indices[0], cuint len indices, "")
  echo "result = ", result, " ", llvm.typeOf(result)

proc gen_bracket_expr(module: BModule; node: PNode): ValueRef =
  let adr = gen_bracket_expr_lvalue(module, node)
  result = llvm.buildLoad(module.ll_builder, adr, "")

# ------------------------------------------------------------------------------

proc gen_deref_expr_lvalue(module: BModule; node: PNode): ValueRef =
  # **value -> *value
  let adr = gen_expr_lvalue(module, node[0])
  result = llvm.buildLoad(module.ll_builder, adr, "deref.lvalue")

proc gen_deref_expr(module: BModule; node: PNode): ValueRef =
  # *value -> value
  let adr = gen_expr(module, node[0])
  result = llvm.buildLoad(module.ll_builder, adr, "deref.rvalue")

proc gen_addr(module: BModule; node: PNode): ValueRef =
  # addr foo
  result = gen_expr_lvalue(module, node[0])

# ------------------------------------------------------------------------------

proc gen_expr_lvalue(module: BModule; node: PNode): ValueRef =
  # L value, always pointer
  echo "gen_expr_lvalue kind: ", node.kind
  case node.kind:
  of nkSym:
    result = gen_sym_expr_lvalue(module, node)
  of nkDotExpr:
    result = gen_dot_expr_lvalue(module, node)
  of nkBracketExpr:
    result = gen_bracket_expr_lvalue(module, node)
  of nkHiddenDeref, nkDerefExpr:
    result = gen_deref_expr_lvalue(module, node)
  else: echo "gen_expr_lvalue: unknown node kind: ", node.kind

  ensure_type_kind(result, PointerTypeKind)

proc gen_expr(module: BModule; node: PNode): ValueRef =
  # R value, can be pointer or value
  echo "gen_expr kind: ", node.kind
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
    result = gen_addr(module, node)
  of nkBracketExpr:
    result = gen_bracket_expr(module, node)
  of nkDerefExpr, nkHiddenDeref:
    result = gen_deref_expr(module, node)
  of nkDotExpr:
    result = gen_dot_expr(module, node)
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
      discard gen_proc(module, node.sons[namePos].sym)
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
