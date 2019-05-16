from transf import transformBody
from magicsys import getCompilerProc
from astalgo import debug
import ast, types
import llvm_dll as llvm
import llvm_data, llvm_type, llvm_expr, llvm_abi, llvm_aux

proc gen_importc_proc(module: BModule; sym: PSym): ValueRef =
  result = module.get_value(sym.id)
  if result == nil:
    let proc_type = get_proc_type(module, sym.typ)
    let proc_name = sym.name.s
    result = llvm.addFunction(module.ll_module, proc_name, proc_type)
    llvm.setFunctionCallConv(result, cuint map_call_conv(module, sym.typ.callConv))
    module.add_value(sym.id, result)

# ------------------------------------------------------------------------------

proc gen_proc_prototype*(module: BModule; sym: PSym): ValueRef =
  result = module.get_value(sym.id)
  if result == nil:
    let proc_type  = get_proc_type(module, sym.typ)
    let proc_name  = mangle_proc_name(module, sym)
    let proc_val   = llvm.addFunction(module.ll_module, proc_name, proc_type)
    llvm.setFunctionCallConv(proc_val, cuint map_call_conv(module, sym.typ.callConv))
    module.add_value(sym.id, proc_val)
    result = proc_val

proc gen_proc_body*(module: BModule; sym: PSym): ValueRef =
  result = module.get_value(sym.id)
  if result == nil:
    echo "***********************************************************"
    echo "* generate proc: ", sym.name.s
    echo "* flags: ", sym.flags
    echo "* loc kind: ", sym.loc.k
    echo "* loc flags: ", sym.loc.flags
    echo "* call conv: ", sym.typ.callConv
    echo "***********************************************************"

    # save current bb for nested procs
    let incoming_bb = llvm.getInsertBlock(module.ll_builder)

    let proc_type  = get_proc_type(module, sym.typ)
    let proc_name  = mangle_proc_name(module, sym)
    let proc_val   = llvm.addFunction(module.ll_module, proc_name, proc_type)
    let ret_type   = getReturnType(sym)
    let entry_bb   = llvm.appendBasicBlockInContext(module.ll_context, proc_val, "entry")
    let return_bb  = llvm.appendBasicBlockInContext(module.ll_context, proc_val, "return")
    var ret_addr: ValueRef # addres of *result* variable

    llvm.setFunctionCallConv(proc_val, cuint map_call_conv(module, sym.typ.callConv))

    llvm.setLinkage(proc_val, ExternalLinkage)
    llvm.setVisibility(proc_val, DefaultVisibility)

    let abi = get_abi(module)

    module.add_value(sym.id, proc_val)

    module.open_scope()
    module.top_scope.proc_val = proc_val
    module.top_scope.return_target = return_bb

    llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)

    # - - - - - generate result parameter - - - - -

    var ast_param_index = 0
    var ir_param_index = 0

    var ret_info: ArgInfo
    if ret_type == nil:
      discard
    else:
      ret_info = abi.classify_return_type(module, ret_type)

      case ret_info.class:

      of ArgClass.Direct:
        ret_addr = build_entry_alloca(module, get_proc_param_type(module, ret_type), "result")
        module.add_value(sym.ast.sons[resultPos].sym.id, ret_addr)
        gen_default_init(module, ret_type, ret_addr)

        if ArgFlag.Sext in ret_info.flags: llvm.addAttributeAtIndex(proc_val, AttributeIndex 0, module.ll_signext)
        if ArgFlag.Zext in ret_info.flags: llvm.addAttributeAtIndex(proc_val, AttributeIndex 0, module.ll_zeroext)

      of ArgClass.Indirect:
        ret_addr = llvm.getParam(proc_val, cuint 0)
        llvm.set_value_name(ret_addr, "result")
        module.add_value(sym.ast.sons[resultPos].sym.id, ret_addr)

        gen_default_init(module, ret_type, ret_addr)

        llvm.addAttributeAtIndex(proc_val, AttributeIndex 1, module.ll_sret)

        inc ir_param_index

      of ArgClass.Expand:
        ret_addr = build_entry_alloca(module, get_proc_param_type(module, ret_type), "result")
        module.add_value(sym.ast.sons[resultPos].sym.id, ret_addr)

        gen_default_init(module, ret_type, ret_addr)

      of ArgClass.Ignore:
        discard

      of ArgClass.OpenArray:
        discard

    # - - - - -  geberate formal parameters - - - - -

    for ast_param in sym.typ.n.sons[1 .. ^1]:
      let arg_info = abi.classify_argument_type(module, ast_param.typ)

      echo "param: ", ast_param.sym.name.s, ", type: ", ast_param.typ.kind, ", class: ", arg_info.class

      case arg_info.class

      of ArgClass.Direct:
        let ll_type   = get_proc_param_type(module, ast_param.typ)
        let local_adr = build_entry_alloca(module, ll_type, "param." & ast_param.sym.name.s)
        let value     = llvm.getParam(proc_val, cuint ir_param_index)
        discard llvm.buildStore(module.ll_builder, value, local_adr)
        module.add_value(ast_param.sym.id, local_adr)
        llvm.set_value_name(value, ast_param.sym.name.s)

        if ArgFlag.Sext in arg_info.flags: llvm.addAttributeAtIndex(proc_val, AttributeIndex ir_param_index + 1, module.ll_signext)
        if ArgFlag.Zext in arg_info.flags: llvm.addAttributeAtIndex(proc_val, AttributeIndex ir_param_index + 1, module.ll_zeroext)

        inc ir_param_index

      of ArgClass.Indirect:
        llvm.addAttributeAtIndex(proc_val, AttributeIndex ir_param_index + 1, module.ll_byval)
        let value = llvm.getParam(proc_val, cuint ir_param_index)
        module.add_value(ast_param.sym.id, value)
        llvm.set_value_name(value, ast_param.sym.name.s)
        inc ir_param_index

      of ArgClass.Expand:
        let ll_type   = get_proc_param_type(module, ast_param.typ)
        let local_adr = build_entry_alloca(module, ll_type, "param." & ast_param.sym.name.s)
        module.add_value(ast_param.sym.id, local_adr)

        if ExpandToWords in arg_info.flags:
          var expanded     = expand_struct_to_words(module, ast_param.typ)
          let bitcast_type = llvm.structTypeInContext(module.ll_context, addr expanded[0], cuint len expanded, Bool 0)
          let abi_cast     = llvm.buildBitCast(module.ll_builder, local_adr, llvm.pointerType(bitcast_type, 0), "abi_cast")
          for i, item in expanded:
            let value = llvm.getParam(proc_val, cuint ir_param_index)
            llvm.set_value_name(value, ast_param.sym.name.s & "." & $i)

            var indices = [constant(module, 0i32), constant(module, int32 i)]
            let adr = llvm.buildGEP(module.ll_builder, abi_cast, addr indices[0], 2, "")
            discard llvm.buildStore(module.ll_builder, value, adr)

            inc ir_param_index
        else:
          var expanded = expand_struct(module, ll_type)
          for i, item in expanded:
            let value = llvm.getParam(proc_val, cuint ir_param_index)
            llvm.set_value_name(value, ast_param.sym.name.s & "." & $i)

            var indices = [constant(module, 0i32), constant(module, int32 i)]
            let adr = llvm.buildGEP(module.ll_builder, local_adr, addr indices[0], 2, "")
            discard llvm.buildStore(module.ll_builder, value, adr)

            inc ir_param_index

      of ArgClass.Ignore:
        discard

      of ArgClass.OpenArray:
        var struct_fields = [get_proc_param_type(module, ast_param.typ), module.ll_int]
        let ll_type = llvm.structTypeInContext(module.ll_context, addr struct_fields[0], 2, Bool 0)
        let local_adr = build_entry_alloca(module, ll_type, "param." & ast_param.sym.name.s)
        module.add_value(ast_param.sym.id, local_adr)
        let adr_field_data = build_field_ptr(module, local_adr, constant(module, 0i32))
        let adr_field_lengt = build_field_ptr(module, local_adr, constant(module, 1i32))
        let param_data = llvm.getParam(proc_val, cuint ir_param_index + 0)
        let param_length = llvm.getParam(proc_val, cuint ir_param_index + 1)
        discard llvm.buildStore(module.ll_builder, param_data, adr_field_data)
        discard llvm.buildStore(module.ll_builder, param_length, adr_field_lengt)
        inc ir_param_index, 2

      inc ast_param_index
    # end for

    # - - - - - generate body - - - - -

    let new_body = transformBody(module.module_list.graph, sym, cache = false)
    gen_stmt(module, new_body)

    maybe_terminate(module, return_bb)
    llvm.moveBasicBlockAfter(return_bb, llvm.getInsertBlock(module.ll_builder))
    llvm.positionBuilderAtEnd(module.ll_builder, return_bb)

    # - - - - - generate return block - - - - -

    if ret_type == nil:
      discard llvm.buildRetVoid(module.ll_builder)
    else:
      case ret_info.class:

      of ArgClass.Direct:
        discard llvm.buildRet(module.ll_builder, llvm.buildLoad(module.ll_builder, ret_addr, ""))

      of ArgClass.Indirect:
        discard llvm.buildRetVoid(module.ll_builder)

      of ArgClass.Expand:
        if ExpandToWords in ret_info.flags:
          var expanded = expand_struct_to_words(module, ret_type)
          let bitcast_type = llvm.structTypeInContext(module.ll_context, addr expanded[0], cuint len expanded, Bool 0)
          let abi_cast = llvm.buildBitCast(module.ll_builder, ret_addr, type_to_ptr bitcast_type, "abi_cast")
          let value = llvm.buildLoad(module.ll_builder, abi_cast, "")
          discard llvm.buildRet(module.ll_builder, value)
        else:
          assert false

      of ArgClass.Ignore:
        discard

      of ArgClass.OpenArray:
        discard

    module.close_scope()

    llvm.positionBuilderAtEnd(module.ll_builder, incoming_bb)

    result = proc_val

    echo "########################### VERIFYING PROC ###################################"
    #viewFunctionCFG(proc_val)
    discard verifyFunction(proc_val, PrintMessageAction)
    echo ""
    echo "******* end gen_proc: ", sym.name.s, " *******"

proc gen_proc*(module: BModule; sym: PSym): ValueRef =
  if (sfBorrow in sym.flags) or (sym.typ == nil): return

  if sfImportc in sym.flags:
    result = gen_importc_proc(module, sym)
  else:
    let target = find_module(module, sym)
    discard gen_proc_body(target, sym)
    result = gen_proc_prototype(module, sym)

# ------------------------------------------------------------------------------

proc gen_call_expr*(module: BModule; node: PNode): ValueRef =
  echo "+++++++++++++++++++++++++++++++++++++++++++"
  echo "+ gen_call_expr: ", node[0].sym.name.s
  echo "+ sym.flags: ", node[0].sym.flags
  echo "+++++++++++++++++++++++++++++++++++++++++++"

  let proc_sym   = node[0].sym
  let proc_typ   = node[0].sym.typ
  let ret_type   = proc_typ[0]
  let callee_val = gen_expr(module, node[0])

  var args: seq[ValueRef]

  let abi = get_abi(module)

  var ret_info: ArgInfo

  assert callee_val != nil

  if ret_type == nil:
    discard
  else:
    ret_info = abi.classify_return_type(module, ret_type)

    case ret_info.class

    of ArgClass.Direct:
      discard

    of ArgClass.Indirect:
      result = build_entry_alloca(module, get_proc_param_type(module, ret_type), "call.tmp")
      args.add result

    of ArgClass.Expand:
      result = build_entry_alloca(module, get_proc_param_type(module, ret_type), "call.tmp")

    of ArgClass.Ignore:
      discard

    of ArgClass.OpenArray:
      discard

    # end case

  # *** call arguments ***

  var ast_arg_index = 1
  for ast_arg in node.sons[1 .. ^1]:
    let arg_value = gen_expr(module, ast_arg)
    let arg_type  = ast_arg.typ
    let arg_info  = abi.classify_argument_type(module, arg_type)

    # handle openarray
    if arg_type.kind in {tyArray}:
      let param_type = proc_typ.n.sons[ast_arg_index].typ
      if param_type.kind in {tyOpenArray, tyVarargs}:
        # convert array[T] -> openArray[T]

        # get pointer to array first element. *T
        var indices = [constant(module, 0i32), constant(module, 0i32)]
        let data_ptr = llvm.buildGEP(module.ll_builder, arg_value, addr indices[0], 2, "")

        args.add data_ptr
        args.add constant_int(module, int lengthOrd(module.module_list.config, arg_type))

        inc ast_arg_index
        continue

    case arg_info.class

    of ArgClass.Direct:
      case arg_type.kind:
      of tyBool:
        args.add llvm.buildTrunc(module.ll_builder, arg_value, module.ll_bool, "call_mem_bool_trunc")
      else:
        args.add arg_value

    of ArgClass.Indirect:
      let ll_arg_type = get_proc_param_type(module, arg_type)
      let arg_copy    = build_entry_alloca(module, ll_arg_type, "arg_copy")
      gen_copy(module, arg_copy, arg_value, arg_type)
      args.add arg_copy

    of ArgClass.Expand:
      let ll_arg_type = get_proc_param_type(module, arg_type)
      let arg_copy    = build_entry_alloca(module, ll_arg_type, "arg_copy")
      gen_copy(module, arg_copy, arg_value, arg_type)

      if ExpandToWords in arg_info.flags:
        var expanded     = expand_struct_to_words(module, arg_type)
        let bitcast_type = llvm.structTypeInContext(module.ll_context, addr expanded[0], cuint len expanded, Bool 0)
        let abi_cast     = llvm.buildBitCast(module.ll_builder, arg_copy, type_to_ptr bitcast_type, "abi_cast")
        for i, item in expanded:
          var indices = [constant(module, 0i32), constant(module, int32 i)]
          let adr = llvm.buildGEP(module.ll_builder, abi_cast, addr indices[0], 2, "")
          args.add llvm.buildLoad(module.ll_builder, adr, "")
      else:
        var expanded = expand_struct(module, ll_arg_type)

    of ArgClass.Ignore:
      discard

    of ArgClass.OpenArray:
      discard

    # end case

    inc ast_arg_index

  # end for

  let args_addr = if args.len == 0: nil else: addr args[0]

  if ret_type == nil:
    discard llvm.buildCall(module.ll_builder, callee_val, args_addr, cuint len args, "")
  else:
    case ret_info.class

    of ArgClass.Direct:
      result = llvm.buildCall(module.ll_builder, callee_val, args_addr, cuint len args, "")

    of ArgClass.Indirect:
      discard llvm.buildCall(module.ll_builder, callee_val, args_addr, cuint len args, "")

    of ArgClass.Expand:
      if ExpandToWords in ret_info.flags:
        let call         = llvm.buildCall(module.ll_builder, callee_val, args_addr, cuint len args, "")
        var expanded     = expand_struct_to_words(module, ret_type)
        let bitcast_type = llvm.structTypeInContext(module.ll_context, addr expanded[0], cuint len expanded, Bool 0)
        let abi_cast     = llvm.buildBitCast(module.ll_builder, result, type_to_ptr bitcast_type, "abi_cast")
        for i, item in expanded:
          var indices = [constant(module, 0i32), constant(module, int32 i)]
          let adr = llvm.buildGEP(module.ll_builder, abi_cast, addr indices[0], 2, "")
          let val = llvm.buildExtractValue(module.ll_builder, call, cuint i, "")
          discard llvm.buildStore(module.ll_builder, val, adr)
      else:
        assert false

    of ArgClass.Ignore:
      discard

    of ArgClass.OpenArray:
      discard

    # end case

# ------------------------------------------------------------------------------

proc gen_call_runtime_proc*(module: BModule; name: string) =
  let sym = module.module_list.graph.getCompilerProc(name)

  if sym == nil:
    module.ice("gen_call_runtime_proc: compilerProc missing: " & name)

  let procedure = gen_proc(module, sym)

  # todo: emit call
