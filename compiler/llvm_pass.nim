import ast, idents
from options import ConfigRef
from passes import makePass, skipCodegen
from transf import transformStmt
from modulegraphs import PPassContext, ModuleGraph
from platform import TSystemCPU

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

proc init_codegen(config: ConfigRef) =
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

proc shutdown_codegen =
  llvm.shutdown()

# ------------------------------------------------------------------------------

proc llWriteModules*(backend: RootRef, config: ConfigRef) =
  if ll_load_dll():
    init_codegen(config)
    echo "llWriteModules"
    let mod_list = BModuleList(backend)
    for module in mod_list.modules:
      echo "-- write module --------------------------------"
      echo module.training_wheels
      echo "------------------------------------------------"
    shutdown_codegen()
  else:
    echo "cannot load llvm dll :("

const llPass* = makePass(myOpen, myProcess, myClose)
