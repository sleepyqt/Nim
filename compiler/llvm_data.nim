from modulegraphs import PPassContext, ModuleGraph
from ast import PSym
from options import ConfigRef

type
  BScope* = ref object
    parent*: BScope

  BModule* = ref object of PPassContext
    module_sym*: PSym
    top_scope*: BScope
    module_list*: BModuleList

  BModuleList* = ref object of RootObj
    modules*: seq[BModule]
    graph*: ModuleGraph
    config*: ConfigRef

proc newModuleList*(graph: ModuleGraph): BModuleList =
  new(result)
  result.graph = graph
  result.config = graph.config

proc newModule*(module_list: BModuleList; module_sym: PSym; config: ConfigRef): BModule =
  new(result)
  result.module_sym = module_sym
  result.module_list = module_list
  module_list.modules.add(result)
