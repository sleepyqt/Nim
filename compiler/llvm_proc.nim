from transf import transformBody
from magicsys import getCompilerProc
from astalgo import debug
import ast, types
import llvm_dll as llvm
import llvm_data, llvm_type, llvm_expr, llvm_abi

proc gen_importc_proc(module: BModule; sym: PSym): ValueRef =
  let proc_type = getType(module, sym.typ)
  let proc_name = sym.name.s
  result = llvm.addFunction(module.ll_module, proc_name, proc_type)
  module.add_value(sym.id, result)

# ------------------------------------------------------------------------------

proc gen_proc*(module: BModule; sym: PSym): ValueRef =
  assert sym != nil
  if (sfBorrow in sym.flags) or (sym.typ == nil):
    return
  result = module.get_value(sym.id)
  if result == nil:
    echo "***********************************************************"
    echo "* generate proc: ", sym.name.s
    echo "* flags: ", sym.flags
    echo "* loc kind: ", sym.loc.k
    echo "* loc flags: ", sym.loc.flags
    echo "* call conv: ", sym.typ.callConv
    echo "***********************************************************"

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

    let abi = module.abi

    module.add_value(sym.id, proc_val)

    module.open_scope()
    module.top_scope.proc_val = proc_val
    module.top_scope.return_target = return_bb

    llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)

    # generate formal parameters and implicit *result* variable

    var state = abi_init_func_state(module, abi)
    var ast_param_index = 0
    var ir_param_index = 0

    if ret_type == nil:
      discard
    else:
      case abi_classify_return(module, abi, ret_type, state)

      # passed directly in register
      of ParamClass.Direct:
        ret_addr = insert_entry_alloca(module, get_proc_param_type(module, ret_type), "result")
        module.add_value(sym.ast.sons[resultPos].sym.id, ret_addr)

      # passed by pointer as first argument
      of ParamClass.Indirect:
        llvm.addAttributeAtIndex(proc_val, AttributeIndex 1, module.ll_sret)
        ret_addr = llvm.getParam(proc_val, cuint 0)
        llvm.set_value_name(ret_addr, "result")
        module.add_value(sym.ast.sons[resultPos].sym.id, ret_addr)
        inc ir_param_index

      of ParamClass.Coerce1:
        let coerced = abi_coerce1(module, abi, ret_type)
        ret_addr = insert_entry_alloca(module, get_proc_param_type(module, ret_type), "result")
        module.add_value(sym.ast.sons[resultPos].sym.id, ret_addr)

      of ParamClass.Coerce2:
        let coerced = abi_coerce2(module, abi, ret_type)
        ret_addr = insert_entry_alloca(module, get_proc_param_type(module, ret_type), "result")
        module.add_value(sym.ast.sons[resultPos].sym.id, ret_addr)

      of ParamClass.Extend:
        ret_addr = insert_entry_alloca(module, get_proc_param_type(module, ret_type), "result")
        llvm.addAttributeAtIndex(proc_val, AttributeIndex 0, module.ll_zeroext)
        module.add_value(sym.ast.sons[resultPos].sym.id, ret_addr)

    # *** formal parameters ***

    for ast_param in sym.typ.n.sons[1 .. ^1]:
      let param_class = abi_classify(module, abi, ast_param.typ, state)

      echo "param: ", ast_param.sym.name.s, ", type: ", ast_param.typ.kind, ", class: ", param_class

      case param_class

      # passed directly in register
      of ParamClass.Direct:
        # copy to local variable
        let ll_type   = get_proc_param_type(module, ast_param.typ)
        let local_adr = insert_entry_alloca(module, ll_type, "param." & ast_param.sym.name.s)
        let value     = llvm.getParam(proc_val, cuint ir_param_index)
        discard llvm.buildStore(module.ll_builder, value, local_adr)
        module.add_value(ast_param.sym.id, local_adr)
        llvm.set_value_name(value, ast_param.sym.name.s)
        inc ir_param_index

      # passed indirectly as pointer
      of ParamClass.Indirect:
        llvm.addAttributeAtIndex(proc_val, AttributeIndex ir_param_index + 1, module.ll_byval)
        let value = llvm.getParam(proc_val, cuint ir_param_index)
        module.add_value(ast_param.sym.id, value)
        llvm.set_value_name(value, ast_param.sym.name.s)
        inc ir_param_index

      # coerced into single register
      of ParamClass.Coerce1:
        let coerced   = abi_coerce1(module, abi, ast_param.typ)
        let ll_type   = get_proc_param_type(module, ast_param.typ)
        let local_adr = insert_entry_alloca(module, ll_type, "param." & ast_param.sym.name.s)
        let value     = llvm.getParam(proc_val, cuint ir_param_index)
        let abi_cast  = llvm.buildBitCast(module.ll_builder, local_adr, llvm.pointerType(coerced, 0), "abi_cast")
        discard llvm.buildStore(module.ll_builder, value, abi_cast)
        module.add_value(ast_param.sym.id, local_adr)
        llvm.set_value_name(value, ast_param.sym.name.s)
        inc ir_param_index

      # coerced into two registers
      of ParamClass.Coerce2:
        var coerced   = abi_coerce2(module, abi, ast_param.typ)
        let ll_type   = get_proc_param_type(module, ast_param.typ)
        let local_adr = insert_entry_alloca(module, ll_type, "param." & ast_param.sym.name.s)
        let value0    = llvm.getParam(proc_val, cuint ir_param_index + 0)
        let value1    = llvm.getParam(proc_val, cuint ir_param_index + 1)
        llvm.set_value_name(value0, ast_param.sym.name.s)
        llvm.set_value_name(value1, ast_param.sym.name.s)
        module.add_value(ast_param.sym.id, local_adr)
        let cast_type = llvm.structTypeInContext(module.ll_context, addr coerced[0], cuint 2, Bool 0)
        let abi_cast  = llvm.buildBitCast(module.ll_builder, local_adr, llvm.pointerType(cast_type, 0), "abi_cast")
        var indices0  = [constant(module, 0i32), constant(module, 0i32)]
        var indices1  = [constant(module, 0i32), constant(module, 1i32)]
        let adr0 = llvm.buildGEP(module.ll_builder, abi_cast, addr indices0[0], 2, "")
        let adr1 = llvm.buildGEP(module.ll_builder, abi_cast, addr indices1[0], 2, "")
        discard llvm.buildStore(module.ll_builder, value0, adr0)
        discard llvm.buildStore(module.ll_builder, value1, adr1)
        inc ir_param_index, 2

      # passed directly, zero or sign extended
      of ParamClass.Extend:
        inc ir_param_index

      inc ast_param_index
    # end for

    # generate body
    let new_body = transformBody(module.module_list.graph, sym, cache = false)
    gen_stmt(module, new_body)

    maybe_terminate(module, return_bb)
    llvm.moveBasicBlockAfter(return_bb, llvm.getInsertBlock(module.ll_builder))
    llvm.positionBuilderAtEnd(module.ll_builder, return_bb)

    # generate return block

    if ret_type == nil:
      discard llvm.buildRetVoid(module.ll_builder)
    else:
      case abi_classify_return(module, abi, ret_type, state)

      of ParamClass.Direct:
        discard llvm.buildRet(module.ll_builder, llvm.buildLoad(module.ll_builder, ret_addr, ""))

      of ParamClass.Indirect:
        discard llvm.buildRetVoid(module.ll_builder)

      of ParamClass.Coerce1:
        let coerced  = abi_coerce1(module, abi, ret_type)
        let abi_cast = llvm.buildBitCast(module.ll_builder, ret_addr, llvm.pointerType(coerced, 0), "abi_cast")
        let value    = llvm.buildLoad(module.ll_builder, abi_cast, "")
        discard llvm.buildRet(module.ll_builder, value)

      of ParamClass.Coerce2:
        var coerced  = abi_coerce2(module, abi, ret_type)
        let struct   = llvm.structTypeInContext(module.ll_context, addr coerced[0], cuint 2, Bool false)
        let abi_cast = llvm.buildBitCast(module.ll_builder, ret_addr, llvm.pointerType(struct, 0), "abi_cast")
        let value    = llvm.buildLoad(module.ll_builder, abi_cast, "")
        discard llvm.buildRet(module.ll_builder, value)

      of ParamClass.Extend:
        # todo
        discard llvm.buildRet(module.ll_builder, llvm.buildLoad(module.ll_builder, ret_addr, ""))

    module.close_scope()

    llvm.positionBuilderAtEnd(module.ll_builder, incoming_bb)

    result = proc_val

    echo "******* verifying proc ***********************"
    #viewFunctionCFG(proc_val)
    discard verifyFunction(proc_val, PrintMessageAction)
    echo "******* end gen_proc: ", sym.name.s, " *******"

# ------------------------------------------------------------------------------

proc gen_call_expr*(module: BModule; node: PNode): ValueRef =
  echo ""
  echo "*** gen_call_expr ***"

  #debug node[0].sym.ast

  var args: seq[ValueRef]
  let proc_sym   = node[0].sym
  let ret_type   = getReturnType(proc_sym)
  let callee_val = gen_expr(module, node[0])

  let abi = module.abi

  assert callee_val != nil

  var state = abi_init_func_state(module, abi)

  if ret_type == nil:
    discard
  else:
    case abi_classify_return(module, abi, ret_type, state)

    of ParamClass.Direct:
      discard

    of ParamClass.Indirect:
      result = insert_entry_alloca(module, get_proc_param_type(module, ret_type), "call.tmp")
      args.add result

    of ParamClass.Extend:
      discard

    of ParamClass.Coerce1:
      result = insert_entry_alloca(module, get_proc_param_type(module, ret_type), "call.tmp")

    of ParamClass.Coerce2:
      result = insert_entry_alloca(module, get_proc_param_type(module, ret_type), "call.tmp")

    # end case

  # *** call arguments ***

  var ast_arg_index = 1
  for ast_arg in node.sons[1 .. ^1]:
    let arg_value  = gen_expr(module, ast_arg)
    let arg_type   = ast_arg.typ

    case abi_classify(module, abi, arg_type, state)

    of ParamClass.Direct:
      args.add arg_value

    of ParamClass.Indirect:
      if arg_type.kind in {tyArray}:
        let param_type = proc_sym.typ.n.sons[ast_arg_index].typ
        # check if array need to be converted into openArray
        if param_type.kind in {tyOpenArray}:
          let ll_param_type = get_proc_param_type(module, param_type)

          # allocate struct for openArray
          let arg_open_array = insert_entry_alloca(module, ll_param_type, "arg_open_array")

          let elem_count = cuint lengthOrd(module.module_list.config, arg_type)

          var indices0 = [constant(module, 0i32), constant(module, 0i32)]
          var indices1 = [constant(module, 0i32), constant(module, 1i32)]

          # get pointer to array first element. *T
          let array_ptr = llvm.buildGEP(module.ll_builder, arg_value, addr indices0[0], 2, "")

          # get pointers to openArray struct fields
          # pointer to data. *T
          let adr0 = llvm.buildGEP(module.ll_builder, arg_open_array, addr indices0[0], 2, "")

          # pointer to length. *int
          let adr1 = llvm.buildGEP(module.ll_builder, arg_open_array, addr indices1[0], 2, "")

          discard llvm.buildStore(module.ll_builder, array_ptr, adr0)
          discard llvm.buildStore(module.ll_builder, constant(module, int(elem_count)), adr1)
          args.add arg_open_array
        else:
          let ll_arg_type = get_proc_param_type(module, arg_type)
          let arg_copy    = insert_entry_alloca(module, ll_arg_type, "arg_copy")
          gen_copy(module, arg_copy, arg_value, arg_type)
          args.add arg_copy
      else:
        let ll_arg_type = get_proc_param_type(module, arg_type)
        let arg_copy    = insert_entry_alloca(module, ll_arg_type, "arg_copy")
        gen_copy(module, arg_copy, arg_value, arg_type)
        args.add arg_copy

    of ParamClass.Extend:
      args.add arg_value # todo

    of ParamClass.Coerce1:
      let coerced     = abi_coerce1(module, abi, arg_type)
      let ll_arg_type = get_proc_param_type(module, arg_type)
      let arg_copy    = insert_entry_alloca(module, ll_arg_type, "arg_copy")
      gen_copy(module, arg_copy, arg_value, arg_type)
      let abi_cast    = llvm.buildBitCast(module.ll_builder, arg_copy, llvm.pointerType(coerced, 0), "abi_cast")
      args.add llvm.buildLoad(module.ll_builder, abi_cast, "")

    of ParamClass.Coerce2:
      var coerced     = abi_coerce2(module, abi, arg_type)
      let ll_arg_type = get_proc_param_type(module, arg_type)
      let arg_copy    = insert_entry_alloca(module, ll_arg_type, "arg_copy")
      gen_copy(module, arg_copy, arg_value, arg_type)
      let cast_type   = llvm.structTypeInContext(module.ll_context, addr coerced[0], cuint 2, Bool 0)
      let abi_cast    = llvm.buildBitCast(module.ll_builder, arg_copy, llvm.pointerType(cast_type, 0), "abi_cast")
      var indices0    = [constant(module, 0i32), constant(module, 0i32)]
      var indices1    = [constant(module, 0i32), constant(module, 1i32)]
      let adr0        = llvm.buildGEP(module.ll_builder, abi_cast, addr indices0[0], 2, "")
      let adr1        = llvm.buildGEP(module.ll_builder, abi_cast, addr indices1[0], 2, "")
      args.add llvm.buildLoad(module.ll_builder, adr0, "")
      args.add llvm.buildLoad(module.ll_builder, adr1, "")

    # end case

    inc ast_arg_index

  # end for

  let args_addr = if args.len == 0: nil else: addr args[0]

  if ret_type == nil:
    discard llvm.buildCall(module.ll_builder, callee_val, args_addr, cuint len args, "")
  else:
    case abi_classify_return(module, abi, ret_type, state)

    of ParamClass.Direct:
      result = llvm.buildCall(module.ll_builder, callee_val, args_addr, cuint len args, "")

    of ParamClass.Indirect:
      discard llvm.buildCall(module.ll_builder, callee_val, args_addr, cuint len args, "")

    of ParamClass.Extend:
      result = llvm.buildCall(module.ll_builder, callee_val, args_addr, cuint len args, "")

    of ParamClass.Coerce1:
      let call     = llvm.buildCall(module.ll_builder, callee_val, args_addr, cuint len args, "")
      let coerced  = abi_coerce1(module, abi, ret_type)
      let abi_cast = llvm.buildBitCast(module.ll_builder, result, llvm.pointerType(coerced, 0), "abi_cast")
      discard llvm.buildStore(module.ll_builder, call, abi_cast)

    of ParamClass.Coerce2:
      let call     = llvm.buildCall(module.ll_builder, callee_val, args_addr, cuint len args, "")
      var coerced  = abi_coerce2(module, abi, ret_type)
      let struct   = llvm.structTypeInContext(module.ll_context, addr coerced[0], cuint 2, Bool false)
      let abi_cast = llvm.buildBitCast(module.ll_builder, result, llvm.pointerType(struct, 0), "abi_cast")
      var indices0 = [constant(module, 0i32), constant(module, 0i32)]
      var indices1 = [constant(module, 0i32), constant(module, 1i32)]
      let adr0     = llvm.buildGEP(module.ll_builder, abi_cast, addr indices0[0], 2, "")
      let adr1     = llvm.buildGEP(module.ll_builder, abi_cast, addr indices1[0], 2, "")
      let val0     = llvm.buildExtractValue(module.ll_builder, call, 0, "")
      let val1     = llvm.buildExtractValue(module.ll_builder, call, 1, "")
      discard llvm.buildStore(module.ll_builder, val0, adr0)
      discard llvm.buildStore(module.ll_builder, val1, adr1)

    # end case

# ------------------------------------------------------------------------------

proc gen_call_runtime_proc*(module: BModule; name: string) =
  let sym = module.module_list.graph.getCompilerProc(name)

  if sym == nil:
    module.ice("gen_call_runtime_proc: compilerProc missing: " & name)

  let procedure = gen_proc(module, sym)

  # todo: emit call
