import ast, types, options, tables
from modulegraphs import PPassContext, ModuleGraph, SigHash, hash, `==`, `$`
from sighashes import hashProc, hashNonProc
from msgs import toFullPath, internalError
from lineinfos import FileIndex
from pathutils import AbsoluteFile
from astalgo import getModule
import llvm_dll as llvm

type
  PlatformABI* = enum
    Generic
    AMD64_SystemV
    AMD64_Windows
    X86

  BScope* = ref object
    parent*: BScope
    proc_val*: ValueRef
    break_target*: BasicBlockRef # target for break statement
    break_name*: int # target for named break
    unwind_target*: BasicBlockRef
    return_target*: BasicBlockRef

  BModule* = ref object of PPassContext
    abi*: PlatformABI
    module_sym*: PSym
    top_scope*: BScope
    module_list*: BModuleList
    file_name*: AbsoluteFile
    full_file_name*: AbsoluteFile
    init_proc*: ValueRef # the `main` module procedure
    # cache common types
    ll_void*, ll_mem_bool*, ll_bool*: TypeRef
    ll_char*: TypeRef
    ll_int*, ll_int8*, ll_int16*, ll_int32*, ll_int64*: TypeRef
    ll_float32*, ll_float64*: TypeRef
    ll_cstring*, ll_nim_string*: TypeRef
    ll_pointer*: TypeRef
    ll_generic_seq*: TypeRef
    ll_nim_type*: TypeRef
    # what the ...
    ll_int24*, ll_int40*, ll_int48*, ll_int56*: TypeRef
    # intrisics
    ll_memcpy32*, ll_memcpy64*: TypeRef
    ll_memset32*, ll_memset64*: TypeRef
    # cache for LLVM values and types
    type_cache*: Table[SigHash, TypeRef]
    value_cache*: Table[int, ValueRef]
    # llvm stuff
    ll_context*: ContextRef
    ll_module*: ModuleRef
    ll_builder*: BuilderRef
    ll_machine*: TargetMachineRef
    # attributes
    ll_sret*, ll_byval*, ll_zeroext*, ll_signext*: AttributeRef
    sig_collisions*: CountTable[SigHash]

  BModuleList* = ref object of RootObj
    modules*: seq[BModule]
    closed_modules*: seq[BModule]
    graph*: ModuleGraph
    config*: ConfigRef
    sig_collisions*: CountTable[SigHash]

# ------------------------------------------------------------------------------

proc ice*(module: BModule; message: string) =
  internalError(module.module_list.config, message)

# ------------------------------------------------------------------------------

proc newModuleList*(graph: ModuleGraph): BModuleList =
  new(result)
  result.graph = graph
  result.config = graph.config
  result.sig_collisions = initCountTable[SigHash]()

proc newModule*(module_list: BModuleList; module_sym: PSym; config: ConfigRef): BModule =
  new(result)
  result.module_sym = module_sym
  result.module_list = module_list
  result.type_cache = init_table[SigHash, TypeRef]()
  result.value_cache = init_table[int, ValueRef]()
  result.file_name = AbsoluteFile toFullPath(config, FileIndex module_sym.position)
  result.sig_collisions = initCountTable[SigHash]()

  if module_sym.position >= module_list.modules.len:
    set_len(module_list.modules, module_sym.position + 1)

  module_list.modules[module_sym.position] = result

proc find_module*(module: BModule; sym: PSym): BModule =
  let module_sym = getModule(sym)
  result = module.module_list.modules[module_sym.position]

# Scope Stack ------------------------------------------------------------------

proc open_scope*(module: BModule) =
  var scope = BScope()
  scope.parent = module.top_scope
  module.top_scope = scope

proc close_scope*(module: BModule) =
  module.top_scope = module.top_scope.parent

iterator lookup*(module: BModule): BScope =
  var scope = module.top_scope
  while scope != nil:
    yield scope
    scope = scope.parent

proc proc_scope*(module: BModule): BScope =
  for scope in module.lookup():
    if scope.proc_val != nil:
      return scope

# Symbol Table -----------------------------------------------------------------

proc add_type*(module: BModule; sig: SigHash; typ: TypeRef) =
  module.type_cache.add(sig, typ)

proc get_type*(module: BModule; sig: SigHash): TypeRef =
  module.type_cache.get_or_default(sig)

proc add_value*(module: BModule; id: int; val: ValueRef) =
  module.value_cache.add(id, val)

proc get_value*(module: BModule; id: int): ValueRef =
  module.value_cache.get_or_default(id)

# Name Mangling ----------------------------------------------------------------

proc mangle*(name: string): string =
  name # todo

proc mangle_proc_name*(module: BModule; sym: PSym): string =
  let sig = if sym.kind in routineKinds and sym.typ != nil:
    hashProc(sym)
  else:
    hashNonProc(sym)

  result.add mangle(sym.name.s)
  result.add $sig

  let counter = module.sig_collisions.getOrDefault(sig)
  if counter != 0:
    result.add "_" & $(counter + 1)
  module.sig_collisions.inc(sig)
#[
proc mangle_name*(module: BModule; sym: PSym): string =
  let sig = if sym.kind in routineKinds and sym.typ != nil:
    hashProc(sym)
  else:
    hashNonProc(sym)

  result.add mangle(sym.name.s)
  result.add $sig

  let counter = module.module_list.sig_collisions.getOrDefault(sig)
  if counter != 0:
    result.add "_" & $(counter + 1)
  module.module_list.sig_collisions.inc(sig)
]#
proc mangle_local_name*(module: BModule; sym: PSym): string =
  mangle(sym.name.s)

proc mangle_global_var_name*(module: BModule; sym: PSym): string =
  mangle(sym.name.s)
