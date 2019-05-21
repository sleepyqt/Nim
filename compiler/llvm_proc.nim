from transf import transformBody
from magicsys import getCompilerProc
from astalgo import debug
from ropes import `$`
import ast, types
import llvm_dll as llvm
import llvm_data, llvm_type, llvm_expr, llvm_abi, llvm_aux

const spam = false

proc gen_importc_proc(module: BModule; sym: PSym): ValueRef =
  result = module.get_value(sym)
  if result == nil:
    #let proc_name = $sym.loc.r
    let proc_name = mangle_proc_name(module, sym)
    when spam:
      echo "## --------------------------------------------------"
      echo "## gen_importc_proc : ", sym.name.s, " -> ", proc_name
      echo "## id               : ", sym.id
      echo "## flags            : ", sym.flags
      echo "## loc flags        : ", sym.loc.flags
      echo "## mangled name     : ", proc_name
      echo "## --------------------------------------------------"
    let proc_type = get_proc_type(module, sym.typ)
    result = llvm.addFunction(module.ll_module, proc_name, proc_type)
    llvm.setFunctionCallConv(result, cuint map_call_conv(module, sym.typ.callConv))
    module.add_value(sym, result)

# ------------------------------------------------------------------------------

proc gen_proc_prototype*(module: BModule; sym: PSym): ValueRef =
  result = module.get_value(sym)
  if result == nil:
    let proc_type  = get_proc_type(module, sym.typ)
    let proc_name  = mangle_proc_name(module, sym)
    let proc_val   = llvm.addFunction(module.ll_module, proc_name, proc_type)
    when spam:
      echo "♥♥ --------------------------------------------------"
      echo "♥♥ gen proc proto   : ", sym.name.s
      echo "♥♥ id               : ", sym.id
      echo "♥♥ type             : ", proc_type
      echo "♥♥ flags            : ", sym.flags
      echo "♥♥ loc flags        : ", sym.loc.flags
      echo "♥♥ mangled name     : ", proc_name
      echo "♥♥ --------------------------------------------------"
    llvm.setFunctionCallConv(proc_val, cuint map_call_conv(module, sym.typ.callConv))
    module.add_value(sym, proc_val)
    result = proc_val

proc gen_proc_body*(module: BModule; sym: PSym): ValueRef =
  result = module.get_value(sym)
  if result == nil:
    let proc_name  = mangle_proc_name(module, sym)

    when spam:
      echo "** --------------------------------------------------"
      echo "** generate proc    : ", sym.name.s
      echo "** id               : ", sym.id
      echo "** flags            : ", sym.flags
      echo "** loc kind         : ", sym.loc.k
      echo "** loc flags        : ", sym.loc.flags
      echo "** call conv        : ", sym.typ.callConv
      echo "** mangled name     : ", proc_name
      echo "** --------------------------------------------------"

    #debug sym.ast

    # save current bb for nested procs
    let incoming_bb = llvm.getInsertBlock(module.ll_builder)

    let proc_type  = get_proc_type(module, sym.typ)
    let proc_val   = llvm.addFunction(module.ll_module, proc_name, proc_type)
    let ret_type   = getReturnType(sym)
    let entry_bb   = llvm.appendBasicBlockInContext(module.ll_context, proc_val, "entry")
    let return_bb  = llvm.appendBasicBlockInContext(module.ll_context, proc_val, "return")
    var result_var: ValueRef # addres of *result* variable

    llvm.setFunctionCallConv(proc_val, cuint map_call_conv(module, sym.typ.callConv))

    llvm.setLinkage(proc_val, ExternalLinkage)
    llvm.setVisibility(proc_val, DefaultVisibility)

    let abi = get_abi(module)
    let func_info = abi.get_func_info()

    add_function_attr(module, proc_val, module.ll_noinline)

    if AttrUwtable in func_info.flags:
      add_function_attr(module, proc_val, module.ll_uwtable)

    module.add_value(sym, proc_val)

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
        result_var = build_entry_alloca(module, get_type(module, ret_type), "result")
        module.add_value(sym.ast.sons[resultPos].sym, result_var)
        gen_default_init(module, ret_type, result_var)

        if ArgFlag.Sext in ret_info.flags: llvm.addAttributeAtIndex(proc_val, AttributeIndex 0, module.ll_signext)
        if ArgFlag.Zext in ret_info.flags: llvm.addAttributeAtIndex(proc_val, AttributeIndex 0, module.ll_zeroext)

      of ArgClass.Indirect:
        result_var = llvm.getParam(proc_val, cuint 0)
        llvm.set_value_name(result_var, "result")
        module.add_value(sym.ast.sons[resultPos].sym, result_var)

        gen_default_init(module, ret_type, result_var)

        llvm.addAttributeAtIndex(proc_val, AttributeIndex 1, module.ll_sret)

        inc ir_param_index

      of ArgClass.Expand:
        result_var = build_entry_alloca(module, get_proc_param_type(module, ret_type), "result")
        module.add_value(sym.ast.sons[resultPos].sym, result_var)

        gen_default_init(module, ret_type, result_var)

      of ArgClass.Ignore:
        discard

      of ArgClass.OpenArray:
        discard

    # - - - - -  geberate formal parameters - - - - -

    for ast_param in sym.typ.n.sons[1 .. ^1]:
      let arg_info = abi.classify_argument_type(module, ast_param.typ)

      when spam:
        echo "** param: ", ast_param.sym.name.s, ", type: ", ast_param.typ.kind, ", class: ", arg_info.class

      case arg_info.class

      of ArgClass.Direct:
        let ll_type   = get_proc_param_type(module, ast_param.typ)
        let local_adr = build_entry_alloca(module, ll_type, "param." & ast_param.sym.name.s)
        let value     = llvm.getParam(proc_val, cuint ir_param_index)
        discard llvm.buildStore(module.ll_builder, value, local_adr)
        module.add_value(ast_param.sym, local_adr)
        llvm.set_value_name(value, ast_param.sym.name.s)

        if ArgFlag.Sext in arg_info.flags: llvm.addAttributeAtIndex(proc_val, AttributeIndex ir_param_index + 1, module.ll_signext)
        if ArgFlag.Zext in arg_info.flags: llvm.addAttributeAtIndex(proc_val, AttributeIndex ir_param_index + 1, module.ll_zeroext)

        inc ir_param_index

      of ArgClass.Indirect:
        llvm.addAttributeAtIndex(proc_val, AttributeIndex ir_param_index + 1, module.ll_byval)
        let value = llvm.getParam(proc_val, cuint ir_param_index)
        module.add_value(ast_param.sym, value)
        llvm.set_value_name(value, ast_param.sym.name.s)
        inc ir_param_index

      of ArgClass.Expand:
        let ll_type   = get_proc_param_type(module, ast_param.typ)
        let local_adr = build_entry_alloca(module, ll_type, "param." & ast_param.sym.name.s)
        module.add_value(ast_param.sym, local_adr)

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
        let data = llvm.getParam(proc_val, cuint ir_param_index + 0)
        let length = llvm.getParam(proc_val, cuint ir_param_index + 1)
        let name = "param." & ast_param.sym.name.s
        let local_adr = build_open_array(module, ast_param.typ.elemType, data, length, name)
        module.add_value(ast_param.sym, local_adr)
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
        let result_value = llvm.buildLoad(module.ll_builder, result_var, "")
        if ret_type != nil and ret_type.kind == tyBool:
          discard llvm.buildRet(module.ll_builder, llvm.buildTrunc(module.ll_builder, result_value, module.ll_bool, ""))
        else:
          discard llvm.buildRet(module.ll_builder, result_value)

      of ArgClass.Indirect:
        discard llvm.buildRetVoid(module.ll_builder)

      of ArgClass.Expand:
        if ExpandToWords in ret_info.flags:
          var expanded = expand_struct_to_words(module, ret_type)
          let bitcast_type = llvm.structTypeInContext(module.ll_context, addr expanded[0], cuint len expanded, Bool 0)
          let abi_cast = llvm.buildBitCast(module.ll_builder, result_var, type_to_ptr bitcast_type, "abi_cast")
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

    #echo "###################### VERIFYING PROC ", sym.name.s, " #############################"
    #viewFunctionCFG(proc_val)
    #discard verifyFunction(proc_val, PrintMessageAction)
    #echo ""
    #echo "******* end gen_proc: ", sym.name.s, " *******"

proc gen_proc*(module: BModule; sym: PSym): ValueRef =
  if (sfBorrow in sym.flags) or (sym.typ == nil): return

  if sfImportc in sym.flags:
    result = gen_importc_proc(module, sym)
  else:
    let target = find_module(module, sym)
    discard gen_proc_body(target, sym)
    result = gen_proc_prototype(module, sym)

# ------------------------------------------------------------------------------

proc call_or_invoke(module: BModule; fn: ValueRef; args: ptr ValueRef; numArgs: cuint; name: cstring): ValueRef =
  let try_scope = module.try_scope()
  if try_scope != nil and module.ehmodel != EHModel.LongJump:
    let entry_bb = llvm.getInsertBlock(module.ll_builder)
    let fun = llvm.getBasicBlockParent(entry_bb)
    let then_bb = llvm.appendBasicBlockInContext(module.ll_context, fun, "invoke.then")
    let catch_bb = try_scope.unwind_target
    llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)
    result = llvm.buildInvoke(module.ll_builder, fn, args, numArgs, then_bb, catch_bb, name)
    llvm.moveBasicBlockAfter(then_bb, entry_bb)
    llvm.positionBuilderAtEnd(module.ll_builder, then_bb)
  else:
    result = llvm.buildCall(module.ll_builder, fn, args, numArgs, name)

proc build_call(module: BModule; proc_type: PType; callee: ValueRef; arguments: seq[ValueRef];
                                                                     arguments_types: seq[PType]): ValueRef =
  assert callee != nil
  assert proc_type != nil
  assert proc_type.kind == tyProc
  let abi = get_abi(module)
  let ret_type = proc_type[0]
  var ret_info: ArgInfo
  var ll_args: seq[ValueRef]

  let cc = llvm.getFunctionCallConv(callee)

  if ret_type == nil:
    discard
  else:
    ret_info = abi.classify_return_type(module, ret_type)
    let ll_ret_type = get_proc_param_type(module, ret_type)
    case ret_info.class
    of ArgClass.Direct: discard
    of ArgClass.Indirect: result = build_entry_alloca(module, ll_ret_type, "call.tmp"); ll_args.add result
    of ArgClass.Expand: result = build_entry_alloca(module, ll_ret_type, "call.tmp")
    of ArgClass.Ignore: discard
    of ArgClass.OpenArray: discard

  # *** call arguments ***

  # ............................................................................

  var index = 0
  for arg_value in arguments:
    let arg_type = arguments_types[index]
    let arg_info = abi.classify_argument_type(module, arg_type)

    # handle openarray
    if arg_type.kind in {tyArray}:
      let param_type = proc_type.n.sons[index + 1].typ
      if param_type.kind in {tyOpenArray, tyVarargs}:
        # convert array[T] -> openArray[T]

        # get pointer to array first element. *T
        var indices = [constant(module, 0i32), constant(module, 0i32)]
        let data_ptr = llvm.buildGEP(module.ll_builder, arg_value, addr indices[0], 2, "")

        ll_args.add data_ptr
        ll_args.add constant_int(module, int lengthOrd(module.module_list.config, arg_type))

        inc index
        continue

    # classify arguments
    case arg_info.class
    of ArgClass.Direct:
      case arg_type.kind:
      of tyBool:
        ll_args.add llvm.buildTrunc(module.ll_builder, arg_value, module.ll_bool, "call_mem_bool_trunc")
      else:
        ll_args.add arg_value
    of ArgClass.Indirect:
      let ll_arg_type = get_proc_param_type(module, arg_type)
      let arg_copy    = build_entry_alloca(module, ll_arg_type, "arg_copy")
      gen_copy(module, arg_copy, arg_value, arg_type)
      ll_args.add arg_copy
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
          ll_args.add llvm.buildLoad(module.ll_builder, adr, "")
      else:
        var expanded = expand_struct(module, ll_arg_type)
    of ArgClass.Ignore:
      discard
    of ArgClass.OpenArray:
      #echo "openarray type ", llvm.typeOf(arg_value)
      assert_value_type(arg_value, PointerTypeKind)
      let adr_field_data = build_field_ptr(module, arg_value, constant(module, 0i32))
      let adr_field_lengt = build_field_ptr(module, arg_value, constant(module, 1i32))
      ll_args.add llvm.buildLoad(module.ll_builder, adr_field_data, "")
      ll_args.add llvm.buildLoad(module.ll_builder, adr_field_lengt, "")

    inc index

  # end for ....................................................................

  let args_addr = if ll_args.len == 0: nil else: addr ll_args[0]

  if ret_type == nil:
    let call = call_or_invoke(module, callee, args_addr, cuint len ll_args, "")
    llvm.setInstructionCallConv(call, cc)
  else:
    case ret_info.class
    of ArgClass.Direct:
      result = call_or_invoke(module, callee, args_addr, cuint len ll_args, "")
      llvm.setInstructionCallConv(result, cc)
      if ret_type != nil and ret_type.kind == tyBool:
        result = llvm.buildZExt(module.ll_builder, result, module.ll_mem_bool, "")
    of ArgClass.Indirect:
      let call = call_or_invoke(module, callee, args_addr, cuint len ll_args, "")
      llvm.setInstructionCallConv(call, cc)
    of ArgClass.Expand:
      if ExpandToWords in ret_info.flags:
        let call         = call_or_invoke(module, callee, args_addr, cuint len ll_args, "")
        llvm.setInstructionCallConv(call, cc)
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

proc gen_call_expr*(module: BModule; node: PNode): ValueRef =
  let cc = node[0].typ.callConv

  when spam:
    let sym = if node[0].kind == nkHiddenDeref: node[0][0].sym else: node[0].sym
    echo "++ --------------------------------------------------"
    echo "++ gen_call_expr    : ", sym.name.s, " ", node.kind
    echo "++ sym.flags        : ", sym.flags
    echo "++ call conv        : ", cc
    echo "++ --------------------------------------------------"

  var arguments: seq[ValueRef]
  var arguments_types: seq[PType]

  let callee = gen_expr(module, node[0])

  for arg in node.sons[1 .. ^1]:
    let value = gen_expr(module, arg)
    assert value != nil, (block: (debug(arg); "nil"))
    arguments.add(value)
    arguments_types.add(arg.typ)

  result = build_call(module, node[0].typ, callee, arguments, arguments_types)

# ------------------------------------------------------------------------------

proc gen_call_runtime_proc*(module: BModule; name: string; arguments: seq[ValueRef]): ValueRef =
  #echo "gen_call_runtime_proc:"
  let sym = module.module_list.graph.getCompilerProc(name)

  if sym == nil:
    module.ice("gen_call_runtime_proc: compilerProc missing: " & name)

  var arguments_types: seq[PType] = sym.typ.sons[1 .. ^1]
  let proc_type = sym.typ
  let callee = gen_proc(module, sym)

  assert arguments.len == arguments_types.len

  result = build_call(module, proc_type, callee, arguments, arguments_types)

proc gen_call_runtime_proc*(module: BModule; node: PNode): ValueRef =
  let proc_sym = node[namePos].sym

  if lfNoDecl notin proc_sym.loc.flags:
    let sym = module.module_list.graph.getCompilerProc($proc_sym.loc.r)

    if sym == nil:
      module.ice("gen_call_runtime_proc: compilerProc missing: " & $proc_sym.loc.r)

    discard gen_proc(module, sym)

  result = gen_call_expr(module, node)

# ------------------------------------------------------------------------------

proc get_eh_personality_proc*(module: BModule): ValueRef =
  if module.ehprocs.personality == nil:
    let sym = module.module_list.graph.getCompilerProc("nim_eh_personality")
    if sym == nil: module.ice("nim_eh_personality missing")
    module.ehprocs.personality = gen_proc(module, sym)
    result = module.ehprocs.personality
