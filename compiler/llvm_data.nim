import ast, types, options, tables
from modulegraphs import PPassContext, ModuleGraph, SigHash, hash, `==`, `$`
from sighashes import hashProc, hashNonProc
from msgs import toFullPath, internalError
from lineinfos import FileIndex
from pathutils import AbsoluteFile
from astalgo import getModule
from ropes import `$`
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
    break_name*: int # target for named break (symbol id)
    unwind_target*: BasicBlockRef
    return_target*: BasicBlockRef # target for return statement

  EHProcs = object
    personality*: ValueRef

  EHModel* = enum
    LongJump
    WindowsSEH
    Itanium

  BModule* = ref object of PPassContext
    abi*: PlatformABI
    ehmodel*: EHModel
    module_sym*: PSym
    top_scope*: BScope
    module_list*: BModuleList
    file_name*: AbsoluteFile
    full_file_name*: AbsoluteFile
    init_proc*: ValueRef # the `main` module procedure
    sig_collisions*: CountTable[SigHash]
    ehprocs*: EHProcs
    delayed_procs*: seq[PSym]
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
    ll_setjmp*, ll_longjmp*: TypeRef
    # cache for LLVM values and types
    type_cache*: Table[SigHash, TypeRef]
    value_cache*: Table[int, ValueRef]
    typeinfo_cache*: Table[SigHash, ValueRef] # RTTI
    # llvm stuff
    ll_context*: ContextRef
    ll_module*: ModuleRef
    ll_builder*: BuilderRef
    ll_machine*: TargetMachineRef
    # attributes
    ll_sret*, ll_byval*, ll_zeroext*, ll_signext*: AttributeRef
    ll_noinline*, ll_noreturn*, ll_nounwind*: AttributeRef
    ll_uwtable*, ll_inlinehint*, ll_alwaysinline*: AttributeRef
    ll_norecurse*: AttributeRef

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
  result.typeinfo_cache = init_table[SigHash, ValueRef]()
  result.file_name = AbsoluteFile toFullPath(config, FileIndex module_sym.position)
  result.sig_collisions = initCountTable[SigHash]()

  if module_sym.position >= module_list.modules.len:
    set_len(module_list.modules, module_sym.position + 1)

  module_list.modules[module_sym.position] = result

proc find_module*(module: BModule; sym: PSym): BModule =
  let module_sym = getModule(sym)
  result = module.module_list.modules[module_sym.position]

# Name Mangling ----------------------------------------------------------------

proc mangle*(name: string): string =
  name # todo

proc mangle_proc_name*(module: BModule; sym: PSym): string =
  if sfExportc in sym.flags or sfImportc in sym.flags:
    result = $sym.loc.r
  else:
    let sig = hashProc(sym)

    result.add mangle(sym.name.s) & $sig

    let counter = module.sig_collisions.getOrDefault(sig)
    if counter != 0: (result.add "_" & $(counter + 1))
    module.sig_collisions.inc(sig)

  assert result != ""

proc mangle_local_var_name*(module: BModule; sym: PSym): string =
  mangle(sym.name.s)

proc mangle_global_var_name*(module: BModule; sym: PSym): string =
  if sfExportc in sym.flags or sfImportc in sym.flags:
    result = $sym.loc.r
  else:
    let sig = hashNonProc(sym)

    result.add mangle(sym.name.s) & $sig

    let counter = module.sig_collisions.getOrDefault(sig)
    if counter != 0: (result.add "_" & $(counter + 1))
    module.sig_collisions.inc(sig)

  assert result != ""

proc mangle_rtti_name*(module: BModule; typ: PType; sig: SigHash): string =
  result = "RTTI." & $typ.kind & "." & $sig

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

proc try_scope*(module: BModule): BScope =
  for scope in module.lookup():
    if scope.unwind_target != nil:
      return scope

# Symbol Table -----------------------------------------------------------------

proc add_type*(module: BModule; sig: SigHash; typ: TypeRef) =
  module.type_cache.add(sig, typ)

proc get_type*(module: BModule; sig: SigHash): TypeRef =
  module.type_cache.get_or_default(sig)

proc add_value*(module: BModule; sym: PSym; val: ValueRef) =
  module.value_cache.add(sym.id, val)

proc get_value*(module: BModule; sym: PSym): ValueRef =
  result = module.value_cache.get_or_default(sym.id)

  if result == nil:
    # same symbol may get multiple ids. Example: `newString`
    if sym.kind == skProc and sfImportc in sym.flags:
      let name = mangle_proc_name(module, sym)
      result = llvm.getNamedFunction(module.ll_module, name)

    if sym.kind == skVar and sfImportc in sym.flags:
      let name = mangle_global_var_name(module, sym)
      result = llvm.getNamedGlobal(module.ll_module, name)


proc add_type_info*(module: BModule; sig: SigHash; typ: ValueRef) =
  module.type_info_cache.add(sig, typ)

proc get_type_info*(module: BModule; sig: SigHash): ValueRef =
  module.type_info_cache.get_or_default(sig)
