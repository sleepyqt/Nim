import ast, idents
from options import ConfigRef, completeGeneratedFilePath, withPackageName, CFile
from passes import makePass, skipCodegen
from transf import transformStmt
from modulegraphs import PPassContext, ModuleGraph
from platform import TSystemCPU
from pathutils import changeFileExt, `$`
from extccomp import addFileToCompile
import llvm_data, llvm_expr
import llvm_dll as llvm

#[

list of fronted bugs:
* https://github.com/nim-lang/Nim/issues/4176

]#

proc myOpen(graph: ModuleGraph; module_sym: PSym): PPassContext =
  echo "myOpen ", module_sym.name.s
  if graph.backend == nil:
    graph.backend = newModuleList(graph)
  result = newModule(BModuleList(graph.backend), module_sym, graph.config)

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

  if true or module.module_sym.name.s == "hello": # todo skip for debugging
    if not skipCodegen(module.module_list.config, node):
      let new_node = transformStmt(module.module_list.graph, module.module_sym, node)
      gen_stmt(module, new_node)

# Codegen Setup ----------------------------------------------------------------

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

proc llWriteModules*(backend: RootRef, config: ConfigRef) =
  echo "llWriteModules"

  when true:
    let mod_list = BModuleList(backend)

    #for index, module in mod_list.modules:
    #  echo "finishing module: ", index, " ", module == nil
    #  finish_module(module)

    #for module in mod_list.modules:
    #  if sfMainModule in module.module_sym.flags:
    #    gen_main_module(module)

    for module in mod_list.closed_modules:

      when false:
        let pm = llvm.createPassManager()
        llvm.addPromoteMemoryToRegisterPass(pm)
        llvm.addInstructionCombiningPass(pm)
        llvm.addReassociatePass(pm)
        llvm.addNewGVNPass(pm)
        llvm.addCFGSimplificationPass(pm)
        discard llvm.runPassManager(pm, module.ll_module)

      let config = module.module_list.config
      let base_file_name = completeGeneratedFilePath(config, withPackageName(config, module.file_name))
      let ll_file = changeFileExt(base_file_name, ".ll")
      let bc_file = changeFileExt(base_file_name, ".bc")
      let o_file = changeFileExt(base_file_name, ".o")
      echo "writing module :: ", ll_file, " × ", bc_file, " x ", o_file
      let m = printModuleToString(module.ll_module)
      #echo m
      disposeMessage(m)

      let cf = CFile(obj: o_file)
      addFileToCompile(config, cf)

      when true:
        var err0: cstring
        var res0 = llvm.printModuleToFile(module.ll_module, cstring ll_file, addr err0)
        llvm.disposeMessage(err0)

      when false:
        var res1 = llvm.writeBitcodeToFile(module.ll_module, cstring bc_file)

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

      echo "------------------------------------------------"

const llPass* = makePass(myOpen, myProcess, myClose)
