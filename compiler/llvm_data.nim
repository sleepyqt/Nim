from modulegraphs import PPassContext, ModuleGraph
from ast import PSym, routineKinds
from options import ConfigRef
from sighashes import SigHash, hash, `==`, hashProc, hashNonProc, `$`
from msgs import toFullPath
from lineinfos import FileIndex
from pathutils import AbsoluteFile
import tables
import llvm_dll as llvm

type
  BScope* = ref object
    parent*: BScope
    proc_val*: ValueRef
    break_target*: BasicBlockRef # target for break statement
    break_name*: int # target for named break
    unwind_target*: BasicBlockRef
    return_target*: BasicBlockRef

  BModule* = ref object of PPassContext
    module_sym*: PSym
    top_scope*: BScope
    module_list*: BModuleList
    file_name*: AbsoluteFile
    full_file_name*: AbsoluteFile
    init_proc*: ValueRef # the `main` module procedure
    # cache common types
    ll_void*, ll_bool*, ll_logic_bool*: TypeRef
    ll_char*: TypeRef
    ll_int*, ll_int8*, ll_int16*, ll_int32*, ll_int64*: TypeRef
    ll_float32*, ll_float64*: TypeRef
    ll_cstring*, ll_nim_string*: TypeRef
    ll_pointer*: TypeRef
    # cache for LLVM values and types
    type_cache*: Table[SigHash, TypeRef]
    value_cache*: Table[int, ValueRef]
    # llvm stuff
    ll_context*: ContextRef
    ll_module*: ModuleRef
    ll_builder*: BuilderRef
    ll_machine*: TargetMachineRef

  BModuleList* = ref object of RootObj
    modules*: seq[BModule]
    graph*: ModuleGraph
    config*: ConfigRef
    sig_collisions*: CountTable[SigHash]

# ------------------------------------------------------------------------------

proc newModuleList*(graph: ModuleGraph): BModuleList =
  new(result)
  result.graph = graph
  result.config = graph.config
  result.sig_collisions = initCountTable[SigHash]()

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
  module.ll_char = llvm.int8TypeInContext(module.ll_context)
  module.ll_bool = llvm.int8TypeInContext(module.ll_context)
  module.ll_logic_bool = llvm.int1TypeInContext(module.ll_context)
  module.ll_int = llvm.int64TypeInContext(module.ll_context) # todo platform specific
  module.ll_int8 = llvm.int8TypeInContext(module.ll_context)
  module.ll_int16 = llvm.int16TypeInContext(module.ll_context)
  module.ll_int32 = llvm.int32TypeInContext(module.ll_context)
  module.ll_int64 = llvm.int64TypeInContext(module.ll_context)
  module.ll_float32 = llvm.floatTypeInContext(module.ll_context)
  module.ll_float64 = llvm.doubleTypeInContext(module.ll_context)
  module.ll_cstring = llvm.pointerType(module.ll_char, 0)
  module.ll_pointer = llvm.pointerType(module.ll_void, 0)
  # module.ll_nim_string = todo

  assert(module.ll_context != nil)
  assert(module.ll_builder != nil)
  assert(module.ll_module != nil)
  assert(module.ll_machine != nil)

proc setup_module(module: BModule) =
  module.init_proc = llvm.addFunction(
    module.ll_module,
    module.module_sym.name.s & "_module_main",
    llvm.functionType(module.ll_void, nil, 0, Bool 0))

  let entry_bb = llvm.appendBasicBlockInContext(
    module.ll_context,
    module.init_proc,
    "module_entry")

  llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)
  module.top_scope = BScope(proc_val: module.init_proc, parent: nil)

proc newModule*(module_list: BModuleList; module_sym: PSym; config: ConfigRef): BModule =
  new(result)
  result.module_sym = module_sym
  result.module_list = module_list
  result.type_cache = init_table[SigHash, TypeRef]()
  result.value_cache = init_table[int, ValueRef]()
  result.file_name = AbsoluteFile toFullPath(config, FileIndex module_sym.position)
  setup_codegen(result)
  setup_module(result)
  module_list.modules.add(result)

# Scope Stack ------------------------------------------------------------------

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

proc proc_scope*(module: BModule): BScope =
  for scope in module.lookup():
    if scope.proc_val != nil:
      return scope

# Symbol Table -----------------------------------------------------------------

proc add_type*(module: BModule; sig: SigHash; typ: TypeRef) =
  module.type_cache.add(sig, typ)

proc get_type*(module: BModule; sig: SigHash): TypeRef =
  module.type_cache[sig]

proc add_value*(module: BModule; id: int; val: ValueRef) =
  module.value_cache.add(id, val)

proc get_value*(module: BModule; id: int): ValueRef =
  module.value_cache[id]

# Name Mangling ----------------------------------------------------------------

proc mangle*(name: string): string =
  name # todo

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
