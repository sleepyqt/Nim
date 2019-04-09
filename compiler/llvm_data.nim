from modulegraphs import PPassContext, ModuleGraph
import ast
from options import ConfigRef
from sighashes import SigHash, hash, `==`, hashProc, hashNonProc, `$`
from msgs import toFullPath
from lineinfos import FileIndex
from pathutils import AbsoluteFile
from platform import TSystemCPU, TSystemOS
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
  let config = module.module_list.config

  block:
    module.ll_context = llvm.contextCreate()
    module.ll_builder = llvm.createBuilderInContext(module.ll_context)
    module.ll_module = llvm.moduleCreateWithName(module.module_sym.name.s)

  block:
    var triple: string
    var cpu: cstring

    case config.target.targetCPU:
    of cpuI386:    triple.add "i686-"; cpu = "i686"
    of cpuAMD64:   triple.add "x86_64-"; cpu = "i686"
    else: assert(false, "unsupported CPU")

    case config.target.targetOS:
    of osWindows:  (triple.add "pc-"; triple.add "win32-"; triple.add "gnu")
    of osLinux:    (triple.add "pc-"; triple.add "linux-"; triple.add "gnu")
    of osMacosx:   (triple.add "apple-"; triple.add "darwin")
    else: assert(false, "unsupported OS")

    echo "target triple: ", triple

    var target: llvm.TargetRef = nil
    var err: cstring
    var features: cstring = ""
    if llvm.getTargetFromTriple(triple, addr target, addr err) != 0:
      echo "getTargetFromTriple error: ", err
      llvm.disposeMessage(err)
    var opt_level = llvm.CodeGenLevelNone
    var reloc = llvm.RelocDefault
    var model = llvm.CodeModelDefault
    module.ll_machine = llvm.createTargetMachine(target, triple, cpu, features, opt_level, reloc, model)

  block:
    module.ll_void = llvm.voidTypeInContext(module.ll_context)
    module.ll_char = llvm.int8TypeInContext(module.ll_context)
    module.ll_bool = llvm.int8TypeInContext(module.ll_context)
    module.ll_logic_bool = llvm.int1TypeInContext(module.ll_context)
    if config.target.intSize == 8:
      module.ll_int = llvm.int64TypeInContext(module.ll_context)
    elif config.target.intSize == 4:
      module.ll_int = llvm.int32TypeInContext(module.ll_context)
    else:
      assert(false, "unsupported int size")
    module.ll_int8 = llvm.int8TypeInContext(module.ll_context)
    module.ll_int16 = llvm.int16TypeInContext(module.ll_context)
    module.ll_int32 = llvm.int32TypeInContext(module.ll_context)
    module.ll_int64 = llvm.int64TypeInContext(module.ll_context)
    module.ll_float32 = llvm.floatTypeInContext(module.ll_context)
    module.ll_float64 = llvm.doubleTypeInContext(module.ll_context)
    module.ll_cstring = llvm.pointerType(module.ll_char, 0)
    module.ll_pointer = llvm.pointerType(module.ll_int8, 0)
    # module.ll_nim_string = todo

  block:
    var args_memcpy32 = [
      module.ll_pointer, # dst
      module.ll_pointer, # src
      module.ll_int32, # len
      module.ll_logic_bool] # isvolatile
    module.ll_memcpy32 = llvm.functionType(
      returnType = module.ll_void,
      paramTypes = addr args_memcpy32[0],
      paramCount = cuint len args_memcpy32,
      isVarArg = Bool 0)
    var args_memcpy64 = [
      module.ll_pointer, # dst
      module.ll_pointer, # src
      module.ll_int64, # len
      module.ll_logic_bool] # isvolatile
    module.ll_memcpy64 = llvm.functionType(
      returnType = module.ll_void,
      paramTypes = addr args_memcpy64[0],
      paramCount = cuint len args_memcpy64,
      isVarArg = Bool 0)
    var args_memset32 = [
      module.ll_pointer, # dest
      module.ll_int8, # val
      module.ll_int32, # len
      module.ll_logic_bool] # isvolatile
    module.ll_memset32 = llvm.functionType(
      returnType = module.ll_void,
      paramTypes = addr args_memset32[0],
      paramCount = cuint len args_memset32,
      isVarArg = Bool 0)
    var args_memset64 = [
      module.ll_pointer, # dest
      module.ll_int8, # val
      module.ll_int64, # len
      module.ll_logic_bool] # isvolatile
    module.ll_memset64 = llvm.functionType(
      returnType = module.ll_void,
      paramTypes = addr args_memset64[0],
      paramCount = cuint len args_memset64,
      isVarArg = Bool 0)

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
  module.type_cache.get_or_default(sig)

proc add_value*(module: BModule; id: int; val: ValueRef) =
  module.value_cache.add(id, val)

proc get_value*(module: BModule; id: int): ValueRef =
  module.value_cache.get_or_default(id)

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

proc mangle_local_name*(module: BModule; sym: PSym): string =
  mangle(sym.name.s)

# Intrisics --------------------------------------------------------------------

proc call_intrisic(module: BModule; name: string; typ: TypeRef; args: openarray[ValueRef]): ValueRef =
  var fn = llvm.getNamedFunction(module.ll_module, name)
  if fn == nil:
    fn = llvm.addFunction(module.ll_module, name, typ)
  result = llvm.buildCall(module.ll_builder, fn, unsafe_addr args[0], cuint len args, "")

proc call_memcpy*(module: BModule; dst, src: ValueRef; len: int64) =
  case module.module_list.config.target.intSize:
  of 8:
    let name = "llvm.memcpy.p0i8.p0i8.i64"
    var args = [
      dst,
      src,
      llvm.constInt(module.ll_int64, culonglong len, Bool 0),
      llvm.constInt(module.ll_logic_bool, culonglong 0, Bool 0)]
    discard call_intrisic(module, name, module.ll_memcpy64, args)
  of 4:
    let name = "llvm.memcpy.p0i8.p0i8.i32"
    var args = [
      dst,
      src,
      llvm.constInt(module.ll_int32, culonglong len, Bool 0),
      llvm.constInt(module.ll_logic_bool, culonglong 0, Bool 0)]
    discard call_intrisic(module, name, module.ll_memcpy32, args)
  else: assert(false)

proc call_memset*(module: BModule; dst: ValueRef; val: int8; len: int64) =
  case module.module_list.config.target.intSize:
  of 8:
    let name = "llvm.memset.p0i8.i64"
    var args = [
      dst,
      llvm.constInt(module.ll_int8, culonglong val, Bool 0),
      llvm.constInt(module.ll_int64, culonglong len, Bool 0),
      llvm.constInt(module.ll_logic_bool, culonglong 0, Bool 0)]
    discard call_intrisic(module, name, module.ll_memset64, args)
  of 4:
    let name = "llvm.memset.p0i8.i32"
    var args = [
      dst,
      llvm.constInt(module.ll_int8, culonglong val, Bool 0),
      llvm.constInt(module.ll_int32, culonglong len, Bool 0),
      llvm.constInt(module.ll_logic_bool, culonglong 0, Bool 0)]
    discard call_intrisic(module, name, module.ll_memset32, args)
  else: assert(false)

# ------------------------------------------------------------------------------

proc get_field_index*(module: BModule; typ: PType; sym: PSym): int =
  let node = typ.n
  if node.kind == nkSym:
    if sym == node.sym:
      result = 0
  elif node.kind == nkRecList:
    for son in node:
      if son.sym == sym:
        return
      else:
        inc result
