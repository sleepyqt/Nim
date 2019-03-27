import ast, idents
from options import ConfigRef
from passes import makePass, skipCodegen
from transf import transformStmt
from modulegraphs import PPassContext, ModuleGraph

import llvm_data, llvm_stmt

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

proc llWriteModules*(backend: RootRef, config: ConfigRef) =
  echo "llWriteModules"
  let mod_list = BModuleList(backend)
  for module in mod_list.modules:
    echo "-- write module --------------------------------"
    echo module.training_wheels
    echo "------------------------------------------------"


const llPass* = makePass(myOpen, myProcess, myClose)
