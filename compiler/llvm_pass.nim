import ast, idents, options
from passes import makePass, skipCodegen
from transf import transformStmt
from modulegraphs import PPassContext, ModuleGraph
from platform import TSystemCPU, TSystemOS, Target
from pathutils import changeFileExt, `$`
from extccomp import addFileToCompile
import llvm_data, llvm_expr, llvm_aux
import llvm_dll as llvm

# Codegen Setup ----------------------------------------------------------------

proc select_target_triple(target: Target): string =
  case target.targetCPU:
  of cpuI386:    result.add "i686-"
  of cpuAMD64:   result.add "x86_64-"
  else: assert false

  case target.targetOS:
  of osWindows:    result.add "pc-windows-msvc"
  of osLinux:      result.add "pc-linux-gnu"
  of osMacosx:     result.add "apple-darwin"
  of osStandalone: result.add "pc-unknown-gnu"
  else: assert(false, "unsupported OS")

proc select_target_abi(target: Target): PlatformABI =
  case target.targetCPU:
  of cpuAMD64:
    case target.targetOS:
    of osLinux:       result = PlatformABI.AMD64_SystemV
    of osWindows:     result = PlatformABI.AMD64_Windows
    of osStandalone:  result = PlatformABI.AMD64_SystemV
    else:             result = PlatformABI.Generic
  of cpuI386:
    case target.targetOS:
    of osLinux:       result = PlatformABI.X86
    of osWindows:     result = PlatformABI.X86
    of osStandalone:  result = PlatformABI.X86
    else:             result = PlatformABI.Generic
  else:               result = PlatformABI.Generic

proc setup_codegen(module: BModule) =
  let config = module.module_list.config

  block:
    module.ll_context = llvm.contextCreate()
    module.ll_builder = llvm.createBuilderInContext(module.ll_context)
    module.ll_module = llvm.moduleCreateWithNameInContext(module.module_sym.name.s, module.ll_context)

  block:
    var triple: string
    var cpu: cstring
    var reloc: RelocMode
    var features: string

    case config.target.targetCPU:
    of cpuI386:  cpu = "i686";   reloc = RelocDefault; features = ""
    of cpuAMD64: cpu = "x86-64"; reloc = RelocPIC;     features = "+sse,+sse2"
    else: assert(false, "unsupported CPU")

    triple = select_target_triple(config.target)
    module.abi = select_target_abi(config.target)

    echo "target triple: ", triple, ", abi: ", module.abi, ", reloc: ", reloc

    var target: llvm.TargetRef = nil
    var err: cstring
    if llvm.getTargetFromTriple(triple, addr target, addr err) != 0:
      module.ice("llvm.getTargetFromTriple error: " & $err)
      llvm.disposeMessage(err)
    var opt_level =
      if optOptimizeSpeed in config.options:
        llvm.CodeGenLevelDefault
      elif optOptimizeSize in config.options:
        llvm.CodeGenLevelLess
      else:
        llvm.CodeGenLevelAggressive

    var model = llvm.CodeModelDefault
    module.ll_machine = llvm.createTargetMachine(target, triple, cpu, features, opt_level, reloc, model)
    let layout = llvm.createTargetDataLayout(module.ll_machine)
    llvm.setTarget(module.ll_module, triple)
    llvm.setModuleDataLayout(module.ll_module, layout)

proc setup_init_proc(module: BModule) =
  module.init_proc = llvm.addFunction(
    module.ll_module,
    module.module_sym.name.s & "_module_main",
    llvm.functionType(module.ll_void, nil, 0, Bool 0))

  add_function_attr(module, module.init_proc, module.ll_noinline)
  add_function_attr(module, module.init_proc, module.ll_uwtable)
  llvm.setFunctionCallConv(module.init_proc, cuint map_call_conv(module, ccDefault))

  # module entry point
  let entry_bb = llvm.appendBasicBlockInContext(module.ll_context, module.init_proc, "module_entry")
  llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)
  module.top_scope = BScope(proc_val: module.init_proc, parent: nil)

proc setup_cache(module: BModule) =
  let config = module.module_list.config

  # basic types

  module.ll_void = llvm.voidTypeInContext(module.ll_context)
  module.ll_char = llvm.int8TypeInContext(module.ll_context)
  module.ll_mem_bool = llvm.int8TypeInContext(module.ll_context)
  module.ll_bool = llvm.int1TypeInContext(module.ll_context)
  if config.target.intSize == 8:
    module.ll_int = llvm.int64TypeInContext(module.ll_context)
  elif config.target.intSize == 4:
    module.ll_int = llvm.int32TypeInContext(module.ll_context)
  else:
    assert(false, "unsupported int size")
  module.ll_int8 = llvm.int8TypeInContext(module.ll_context)
  module.ll_int16 = llvm.int16TypeInContext(module.ll_context)
  module.ll_int32 = llvm.int32TypeInContext(module.ll_context)
  module.ll_int64 = llvm.int64TypeInContext(module.ll_context)
  module.ll_int24 = llvm.intTypeInContext(module.ll_context, 24)
  module.ll_int40 = llvm.intTypeInContext(module.ll_context, 40)
  module.ll_int48 = llvm.intTypeInContext(module.ll_context, 48)
  module.ll_int56 = llvm.intTypeInContext(module.ll_context, 56)
  module.ll_float32 = llvm.floatTypeInContext(module.ll_context)
  module.ll_float64 = llvm.doubleTypeInContext(module.ll_context)
  module.ll_cstring = llvm.pointerType(module.ll_char, 0)
  module.ll_pointer = llvm.pointerType(module.ll_int8, 0)

  # nim string

  module.ll_nim_string = nil
  module.ll_generic_seq = nil
  module.ll_nim_type = nil

  # intrisics

  var args_memcpy32 = [module.ll_pointer, module.ll_pointer, module.ll_int32, module.ll_bool]
  module.ll_memcpy32 = llvm.functionType(
    returnType = module.ll_void,
    paramTypes = addr args_memcpy32[0],
    paramCount = cuint len args_memcpy32,
    isVarArg = Bool 0)
  var args_memcpy64 = [module.ll_pointer, module.ll_pointer, module.ll_int64, module.ll_bool]
  module.ll_memcpy64 = llvm.functionType(
    returnType = module.ll_void,
    paramTypes = addr args_memcpy64[0],
    paramCount = cuint len args_memcpy64,
    isVarArg = Bool 0)
  var args_memset32 = [module.ll_pointer, module.ll_int8, module.ll_int32, module.ll_bool]
  module.ll_memset32 = llvm.functionType(
    returnType = module.ll_void,
    paramTypes = addr args_memset32[0],
    paramCount = cuint len args_memset32,
    isVarArg = Bool 0)
  var args_memset64 = [module.ll_pointer, module.ll_int8, module.ll_int64, module.ll_bool]
  module.ll_memset64 = llvm.functionType(
    returnType = module.ll_void,
    paramTypes = addr args_memset64[0],
    paramCount = cuint len args_memset64,
    isVarArg = Bool 0)

  # attributes

  template get_attr(name, val): AttributeRef =
    let kind_id = llvm.getEnumAttributeKindForName(name, csize len name)
    llvm.createEnumAttribute(module.ll_context, kind_id, val)

  module.ll_sret = get_attr("sret", 0)
  module.ll_byval = get_attr("byval", 0)
  module.ll_zeroext = get_attr("zeroext", 0)
  module.ll_signext = get_attr("signext", 0)
  module.ll_noinline = get_attr("noinline", 0)
  module.ll_noreturn = get_attr("noreturn", 0)
  module.ll_nounwind = get_attr("nounwind", 0)
  module.ll_uwtable = get_attr("uwtable", 0)
  module.ll_inlinehint = get_attr("inlinehint", 0)
  module.ll_alwaysinline = get_attr("alwaysinline", 0)
  module.ll_norecurse = get_attr("norecurse", 0)

proc prepare_module_for_codegen(module: BModule) =
  module.setup_codegen()
  module.setup_cache()
  module.setup_init_proc()

proc llInit*(config: ConfigRef): bool =
  result = ll_load_dll()
  if result:
    case config.target.targetCPU:
    of cpuI386, cpuAMD64:
      llvm.initializeX86Target()
      llvm.initializeX86TargetInfo()
      llvm.initializeX86TargetTargetMC()
      llvm.initializeX86AsmParser()
      llvm.initializeX86AsmPrinter()
      llvm.initializeX86Disassembler()

      let pass_reg = llvm.getGlobalPassRegistry()
      llvm.initializeCore(pass_reg)
      llvm.initializeTransformUtils(pass_reg)
      llvm.initializeInstrumentation(pass_reg)
      llvm.initializeCodeGen(pass_reg)
      llvm.initializeTarget(pass_reg)
      llvm.initializeScalarOpts(pass_reg)
    else: assert(false, "unsupported cpu")

proc llShutdown* =
  llvm.shutdown()

# ------------------------------------------------------------------------------

proc finish_module(module: BModule) =
  let incoming_bb = llvm.getInsertBlock(module.ll_builder)
  let exit_bb = llvm.appendBasicBlockInContext(module.ll_context, module.init_proc, "module_exit")
  llvm.positionBuilderAtEnd(module.ll_builder, incoming_bb)
  discard llvm.buildBr(module.ll_builder, exit_bb)

  llvm.positionBuilderAtEnd(module.ll_builder, exit_bb)
  discard llvm.buildRetVoid(module.ll_builder)

proc get_main_proc(module: BModule): ValueRef =
  let config = module.module_list.config

  case config.target.targetOS:

  of osWindows:
    if optGenGuiApp in config.globalOptions:
      result = llvm.addFunction(
        module.ll_module, "WinMain", llvm.functionType(module.ll_int32, nil, 0, Bool 0))
    else:
      result = llvm.addFunction(
        module.ll_module, "DllMain", llvm.functionType(module.ll_int32, nil, 0, Bool 0))

  of osLinux:
    result = llvm.addFunction(
      module.ll_module, "main", llvm.functionType(module.ll_int32, nil, 0, Bool 0))

  of osStandalone:
    result = llvm.addFunction(
      module.ll_module, "main", llvm.functionType(module.ll_int32, nil, 0, Bool 0))

  else: assert false

  llvm.setFunctionCallConv(result, cuint map_call_conv(module, ccDefault))

proc gen_main_module*(module: BModule) =
  let config = module.module_list.config

  if optNoMain notin config.globalOptions:
    let main_proc = get_main_proc(module)
    let entry_bb = llvm.appendBasicBlockInContext(module.ll_context, main_proc, "app_entry")
    llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)
    let call = llvm.buildCall(module.ll_builder, module.init_proc, nil, 0, "")
    llvm.setInstructionCallConv(call, llvm.getFunctionCallConv(main_proc))
    discard llvm.buildRet(module.ll_builder, llvm.constInt(module.ll_int32, culonglong 0, Bool 0))

    add_function_attr(module, main_proc, module.ll_noinline)
    add_function_attr(module, main_proc, module.ll_uwtable)
    add_function_attr(module, main_proc, module.ll_norecurse)


# - Codegen Pass ---------------------------------------------------------------

proc myOpen(graph: ModuleGraph; module_sym: PSym): PPassContext =
  echo "myOpen ", module_sym.name.s
  if graph.backend == nil:
    graph.backend = newModuleList(graph)
  let module = newModule(BModuleList(graph.backend), module_sym, graph.config)
  prepare_module_for_codegen(module)
  result = module

proc myClose(graph: ModuleGraph; pass: PPassContext, node: PNode): PNode =
  result = node

  if pass != nil:
    let module = BModule(pass)
    echo "myClose ", module.module_sym.name.s
    if not skipCodegen(module.module_list.config, node):
      finish_module(module)
      if sfMainModule in module.module_sym.flags:
        gen_main_module(module)
      module.module_list.closed_modules.add module

proc myProcess(pass: PPassContext, node: PNode): PNode =
  result = node
  var module = BModule(pass)

  if true:
    if not skipCodegen(module.module_list.config, node):
      let new_node = transformStmt(module.module_list.graph, module.module_sym, node)
      gen_stmt(module, new_node)

# ------------------------------------------------------------------------------

proc run_verify_pass(module: BModule): bool =
  var msg: cstring
  result = bool llvm.verifyModule(module.ll_module, llvm.PrintMessageAction, cast[cstringArray](addr msg))

proc run_opt_speed_pass(module: BModule) =
  let pm = llvm.createPassManager()
  llvm.addPromoteMemoryToRegisterPass(pm)
  llvm.addInstructionCombiningPass(pm)
  llvm.addReassociatePass(pm)
  llvm.addNewGVNPass(pm)
  llvm.addCFGSimplificationPass(pm)
  discard llvm.runPassManager(pm, module.ll_module)

proc llWriteModules*(backend: RootRef, config: ConfigRef) =

  when true:
    let mod_list = BModuleList(backend)

    for module in mod_list.closed_modules:
      let config = module.module_list.config
      let base_file_name = completeGeneratedFilePath(config, withPackageName(config, module.file_name))
      let ll_file = changeFileExt(base_file_name, ".ll")
      let bc_file = changeFileExt(base_file_name, ".bc")
      let o_file = changeFileExt(base_file_name, ".o")

      var errors = run_verify_pass(module)

      if optOptimizeSpeed in config.options:
        run_opt_speed_pass(module)

      errors = run_verify_pass(module)

      when false:
        echo "writing module :: ", ll_file, " × ", bc_file, " x ", o_file
        let m = printModuleToString(module.ll_module)
        echo m
        disposeMessage(m)
        echo "---------------------------------------------------"

      let cf = CFile(obj: o_file)
      addFileToCompile(config, cf)

      when true:
        var err0: cstring
        var res0 = llvm.printModuleToFile(module.ll_module, cstring ll_file, addr err0)
        llvm.disposeMessage(err0)

      when false:
        var res1 = llvm.writeBitcodeToFile(module.ll_module, cstring bc_file)

      if errors:
        module.ice("broken bitcode")

      when true:
        var err1: cstring
        var fmt = llvm.ObjectFile
        var res2 = llvm.targetMachineEmitToFile(
          t = module.ll_machine,
          m = module.ll_module,
          filename = cstring o_file,
          codegen = fmt,
          errorMessage = addr err1)
        llvm.disposeMessage(err1)


const llPass* = makePass(myOpen, myProcess, myClose)
