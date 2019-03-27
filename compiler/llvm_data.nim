from modulegraphs import PPassContext, ModuleGraph
from ast import PSym
from options import ConfigRef
from sighashes import SigHash
import tables

import llvm_dll

type
  BScope* = ref object
    parent*: BScope
    proc_val*: ValueRef

  BModule* = ref object of PPassContext
    module_sym*: PSym
    top_scope*: BScope
    module_list*: BModuleList
    # cache common types
    ll_int*, ll_int8*, ll_int16*, ll_int32*, ll_int64*: TypeRef
    ll_float32*, ll_float64*: TypeRef
    # cache for LLVM values and types
    type_cache*: Table[SigHash, TypeRef]
    value_cache*: Table[int, ValueRef]
    # ☺
    training_wheels*: string

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
  result.type_cache = init_table[SigHash, TypeRef]()
  result.value_cache = init_table[int, ValueRef]()
  module_list.modules.add(result)

# scopes

proc open_scope*(module: BModule) =
  var scope = BScope()
  scope.parent = module.top_scope
  module.top_scope = scope

proc close_scope*(module: BModule) =
  module.top_scope = module.top_scope.parent

# cache

proc add_type*(module: BModule; sig: SigHash; typ: TypeRef) =
  discard

proc get_type*(module: BModule; sig: SigHash): TypeRef =
  discard

proc add_value*(module: BModule; id: int; val: ValueRef) =
  discard

proc get_value*(module: BModule; id: int): ValueRef =
  discard
