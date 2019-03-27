import ast, idents
from options import ConfigRef, completeGeneratedFilePath, withPackageName
from passes import makePass, skipCodegen
from transf import transformStmt
from modulegraphs import PPassContext, ModuleGraph
from platform import TSystemCPU
from pathutils import changeFileExt, `$`

import llvm_data, llvm_stmt
import llvm_dll as llvm

proc myOpen(graph: ModuleGraph; module_sym: PSym): PPassContext =
  echo "myOpen ", module_sym.name.s
  if graph.backend == nil:
    graph.backend = newModuleList(graph)
  result = newModule(BModuleList(graph.backend), module_sym, graph.config)

proc myClose(graph: ModuleGraph; pass: PPassContext, node: PNode): PNode =
  result = node

proc myProcess(pass: PPassContext, node: PNode): PNode =
  #echo "myProcess"
  result = node
  var module = BModule(pass)
  if module.module_sym.name.s == "hello": # todo skip for debugging
    if not skipCodegen(module.module_list.config, node):
      let new_node = transformStmt(module.module_list.graph, module.module_sym, node)
      gen_stmt_list(module, new_node)

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
    else: assert(false, "unsupported cpu")

proc llShutdown* =
  llvm.shutdown()

# ------------------------------------------------------------------------------

proc llWriteModules*(backend: RootRef, config: ConfigRef) =
  echo "llWriteModules"

  let mod_list = BModuleList(backend)
  for module in mod_list.modules:
    let config = module.module_list.config
    let base_file_name = completeGeneratedFilePath(config, withPackageName(config, module.file_name))
    let ll_file = changeFileExt(base_file_name, ".ll")
    let bc_file = changeFileExt(base_file_name, ".bc")
    let o_file = changeFileExt(base_file_name, ".o")
    echo "-- write module --------------------------------"
    echo ll_file
    echo bc_file
    echo o_file
    echo module.training_wheels

    when true:
      var err0: cstring
      var res0 = llvm.printModuleToFile(module.ll_module, cstring ll_file, addr err0)
      llvm.disposeMessage(err0)

    when false:
      var res1 = llvm.writeBitcodeToFile(module.ll_module, cstring bc_file)

    when false:
      var err1: cstring
      var fmt = llvm.ObjectFile
      var res2 = llvm.targetMachineEmitToFile(module.ll_machine, cstring module.ll_module, o_file, fmt, addr err1)
      llvm.disposeMessage(err1)

    echo "------------------------------------------------"

const llPass* = makePass(myOpen, myProcess, myClose)
