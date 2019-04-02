from modulegraphs import PPassContext, ModuleGraph
from ast import PSym
from options import ConfigRef
from sighashes import SigHash, hash, `==`
from msgs import toFullPath
from lineinfos import FileIndex
from pathutils import AbsoluteFile
import tables

import llvm_dll as llvm

type
  BScope* = ref object
    parent*: BScope
    proc_val*: ValueRef
    break_target*: BasicBlockRef
    break_name*: int

  BModule* = ref object of PPassContext
    module_sym*: PSym
    top_scope*: BScope
    module_list*: BModuleList
    file_name*: AbsoluteFile
    full_file_name*: AbsoluteFile
    # cache common types
    ll_void*, ll_bool*: TypeRef
    ll_int*, ll_int8*, ll_int16*, ll_int32*, ll_int64*: TypeRef
    ll_float32*, ll_float64*: TypeRef
    # cache for LLVM values and types
    type_cache*: Table[SigHash, TypeRef]
    value_cache*: Table[int, ValueRef]
    # ☺
    training_wheels*: string
    # llvm stuff
    ll_context*: ContextRef
    ll_module*: ModuleRef
    ll_builder*: BuilderRef
    ll_machine*: TargetMachineRef

  BModuleList* = ref object of RootObj
    modules*: seq[BModule]
    graph*: ModuleGraph
    config*: ConfigRef

proc newModuleList*(graph: ModuleGraph): BModuleList =
  new(result)
  result.graph = graph
  result.config = graph.config

proc setup_codegen(module: var BModule) =
  module.ll_context = llvm.contextCreate()
  module.ll_builder = llvm.createBuilderInContext(module.ll_context)
  module.ll_module = llvm.moduleCreateWithName(module.module_sym.name.s)
  # --- setup target ---
  # "i686-pc-linux-gnu"
  # "x86_64-pc-linux-gnu"
  var target: llvm.TargetRef = nil
  var err: cstring
  var triple: cstring = "x86_64-pc-linux-gnu"
  var cpu: cstring = "i686"
  var features: cstring = ""

  if llvm.getTargetFromTriple(triple, addr target, addr err) != 0:
    echo "getTargetFromTriple error: ", err
    llvm.disposeMessage(err)

  var opt_level = llvm.CodeGenLevelNone
  var reloc = llvm.RelocDefault
  var model = llvm.CodeModelDefault
  module.ll_machine = llvm.createTargetMachine(target, triple, cpu, features, opt_level, reloc, model)
  # --- cache types ---
  module.ll_void = llvm.voidTypeInContext(module.ll_context)
  module.ll_bool = llvm.int8TypeInContext(module.ll_context)
  module.ll_int = llvm.int64TypeInContext(module.ll_context) # todo platform specific
  module.ll_int8 = llvm.int8TypeInContext(module.ll_context)
  module.ll_int16 = llvm.int16TypeInContext(module.ll_context)
  module.ll_int32 = llvm.int32TypeInContext(module.ll_context)
  module.ll_int64 = llvm.int64TypeInContext(module.ll_context)
  module.ll_float32 = llvm.floatTypeInContext(module.ll_context)
  module.ll_float64 = llvm.doubleTypeInContext(module.ll_context)

  assert(module.ll_context != nil)
  assert(module.ll_builder != nil)
  assert(module.ll_module != nil)
  assert(module.ll_machine != nil)

proc newModule*(module_list: BModuleList; module_sym: PSym; config: ConfigRef): BModule =
  new(result)
  result.module_sym = module_sym
  result.module_list = module_list
  result.type_cache = init_table[SigHash, TypeRef]()
  result.value_cache = init_table[int, ValueRef]()
  result.file_name = AbsoluteFile toFullPath(config, FileIndex module_sym.position)
  setup_codegen(result)
  module_list.modules.add(result)

# scopes

proc open_scope*(module: BModule) =
  echo "OPEN SCOPE"
  var scope = BScope()
  scope.parent = module.top_scope
  module.top_scope = scope

proc close_scope*(module: BModule) =
  echo "CLOSE SCOPE"
  module.top_scope = module.top_scope.parent

iterator lookup*(module: BModule): BScope =
  var scope = module.top_scope
  while scope != nil:
    yield scope
    scope = scope.parent

# cache

proc add_type*(module: BModule; sig: SigHash; typ: TypeRef) =
  module.type_cache.add(sig, typ)

proc get_type*(module: BModule; sig: SigHash): TypeRef =
  module.type_cache[sig]

proc add_value*(module: BModule; id: int; val: ValueRef) =
  module.value_cache.add(id, val)

proc get_value*(module: BModule; id: int): ValueRef =
  module.value_cache[id]
