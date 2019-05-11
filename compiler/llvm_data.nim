import ast, types
from modulegraphs import PPassContext, ModuleGraph, SigHash, hash, `==`, `$`
from options import ConfigRef
from sighashes import hashProc, hashNonProc
from msgs import toFullPath, internalError
from lineinfos import FileIndex
from pathutils import AbsoluteFile
from platform import TSystemCPU, TSystemOS, Target
from astalgo import getModule

import tables
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
    ll_void*, ll_bool*, ll_logic_bool*: TypeRef
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

proc `$`*(x: TypeRef): string =
  if x == nil:
    result = "nil TypeRef"
  else:
    let str = llvm.printTypeToString(x)
    result = $str
    disposeMessage(str)

proc `$`*(x: ValueRef): string =
  if x == nil:
    result = "nil ValueRef"
  else:
    let str = llvm.printValueToString(x)
    result = $str
    disposeMessage(str)

# ------------------------------------------------------------------------------

template ensure_type_kind*(val: ValueRef; kind: TypeKind) =
  when true:
    assert val != nil
    let typ = llvm.typeOf(val)
    if llvm.getTypeKind(typ) != kind:
      let type_name = llvm.printTypeToString(typ)
      let value_name = llvm.printValueToString(val)
      echo "#### ensure_type_kind fail:"
      echo "#### value = ", value_name
      echo "#### type  = ", type_name
      disposeMessage(type_name)
      disposeMessage(value_name)
      #assert false

# ------------------------------------------------------------------------------

proc constant*(module: BModule; value: int8): ValueRef =
  result = llvm.constInt(module.ll_int8, culonglong value, Bool 1)

proc constant*(module: BModule; value: int16): ValueRef =
  result = llvm.constInt(module.ll_int16, culonglong value, Bool 1)

proc constant*(module: BModule; value: int32): ValueRef =
  result = llvm.constInt(module.ll_int32, culonglong value, Bool 1)

proc constant*(module: BModule; value: int64): ValueRef =
  result = llvm.constInt(module.ll_int64, culonglong value, Bool 1)

proc constant*(module: BModule; value: uint8): ValueRef =
  result = llvm.constInt(module.ll_int8, culonglong value, Bool 0)

proc constant*(module: BModule; value: uint16): ValueRef =
  result = llvm.constInt(module.ll_int16, culonglong value, Bool 0)

proc constant*(module: BModule; value: uint32): ValueRef =
  result = llvm.constInt(module.ll_int32, culonglong value, Bool 0)

proc constant*(module: BModule; value: uint64): ValueRef =
  result = llvm.constInt(module.ll_int64, culonglong value, Bool 0)

proc constant_int*(module: BModule; value: int64): ValueRef =
  case module.module_list.config.target.intSize:
  of 8: result = constant(module, int64 value)
  of 4: result = constant(module, int32 value)
  else: assert false

# ------------------------------------------------------------------------------

proc struct_field_ptr*(module: BModule; struct, index: ValueRef; hint = "struct.index"): ValueRef =
  ensure_type_kind(struct, PointerTypeKind)
  var indices = [constant(module, 0i32), index]
  result = llvm.buildGEP(module.ll_builder, struct, addr indices[0], 2, hint)

# ------------------------------------------------------------------------------

proc is_signed_type*(typ: PType): bool =
  typ.kind in {tyInt .. tyInt64}

proc get_type_size*(module: BModule; typ: PType): BiggestInt =
  result = getSize(module.module_list.config, typ)

proc get_type_align*(module: BModule; typ: PType): BiggestInt =
  result = getAlign(module.module_list.config, typ)

# ------------------------------------------------------------------------------

proc i8_to_i1*(module: BModule; value: ValueRef): ValueRef =
  assert value != nil
  if llvm.getValueKind(value) == InstructionValueKind:
    # eleminate common zext trunc combo
    if llvm.getInstructionOpcode(value) == llvm.ZExt:
      result = llvm.getOperand(value, 0)
      if llvm.getFirstUse(value) == nil:
        llvm.instructionEraseFromParent(value)
      return
  result = llvm.buildTrunc(module.ll_builder, value, module.ll_logic_bool, "")

proc i1_to_i8*(module: BModule; value: ValueRef): ValueRef =
  result = llvm.buildZExt(module.ll_builder, value, module.ll_bool, "")

proc type_to_ptr*(value: TypeRef): TypeRef =
  result = llvm.pointerType(value, 0)

# ------------------------------------------------------------------------------

proc maybe_terminate*(module: BModule; target: BasicBlockRef) =
  # try to terminate current block
  if llvm.getBasicBlockTerminator(getInsertBlock(module.ll_builder)) == nil:
    discard llvm.buildBr(module.ll_builder, target)

proc insert_entry_alloca*(module: BModule; typ: TypeRef; name: cstring): ValueRef =
  # insert `alloca` instruction at function entry point
  let incoming_bb = llvm.getInsertBlock(module.ll_builder)
  let fun         = llvm.getBasicBlockParent(incoming_bb)
  let entry_bb    = llvm.getEntryBasicBlock(fun)
  let first       = llvm.getFirstInstruction(entry_bb)
  if first == nil:
    llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)
  else:
    llvm.positionBuilderBefore(module.ll_builder, first)
  result = llvm.buildAlloca(module.ll_builder, typ, name)
  # restore position
  llvm.positionBuilderAtEnd(module.ll_builder, incoming_bb)

# ------------------------------------------------------------------------------

proc expand_struct*(module: BModule; struct: TypeRef): seq[TypeRef] =
  # {i8, i16, i32} -> i8, i16, i32
  let count = int llvm.countStructElementTypes(struct)
  setLen(result, count)
  llvm.getStructElementTypes(struct, addr result[0])

proc wrap_in_struct*(module: BModule; types: seq[TypeRef]): TypeRef =
  # i8, i16, i32 -> {i8, i16, i32}
  result = llvm.structTypeInContext(module.ll_context, unsafe_addr types[0], cuint len types, Bool false)

proc expand_struct_to_words*(module: BModule; struct: PType): seq[TypeRef] =
  # {i8, i16, i32} -> i32, i32
  let size = get_type_size(module, struct)

  case module.module_list.config.target.intSize:
  of 8:
    case size:
    of  1 .. 8:  result = @[module.ll_int64]
    of  9 .. 16: result = @[module.ll_int64, module.ll_int64]
    of 17 .. 24: result = @[module.ll_int64, module.ll_int64, module.ll_int64]
    of 25 .. 32: result = @[module.ll_int64, module.ll_int64, module.ll_int64, module.ll_int64]
    else: assert false
  of 4:
    case size:
    of 1 .. 4:   result = @[module.ll_int32]
    of 5 .. 8:   result = @[module.ll_int32, module.ll_int32]
    else: assert false
  else:
    assert false

# ------------------------------------------------------------------------------

proc newModuleList*(graph: ModuleGraph): BModuleList =
  new(result)
  result.graph = graph
  result.config = graph.config
  result.sig_collisions = initCountTable[SigHash]()

proc select_target_triple(target: Target): string =
  case target.targetCPU:
  of cpuI386:    result.add "i686-"
  of cpuAMD64:   result.add "x86_64-"
  else: assert false

  case target.targetOS:
  of osWindows:    result.add "pc-windows-msvc"
  of osLinux:      result.add "pc-linux-gnu"
  of osMacosx:     result.add "apple-darwin"
  of osStandalone: result.add "pc-unknown-gnu"
  else: assert(false, "unsupported OS")

proc select_target_abi(target: Target): PlatformABI =
  case target.targetCPU:
  of cpuAMD64:
    case target.targetOS:
    of osLinux:       result = PlatformABI.AMD64_SystemV
    of osWindows:     result = PlatformABI.AMD64_Windows
    of osStandalone:  result = PlatformABI.AMD64_SystemV
    else:             result = PlatformABI.Generic
  of cpuI386:
    case target.targetOS:
    of osLinux:       result = PlatformABI.X86
    of osWindows:     result = PlatformABI.X86
    of osStandalone:  result = PlatformABI.X86
    else:             result = PlatformABI.Generic
  else:               result = PlatformABI.Generic

proc cache_types(module: BModule) =
  let config = module.module_list.config

  # basic types

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
  module.ll_int24 = llvm.intTypeInContext(module.ll_context, 24)
  module.ll_int40 = llvm.intTypeInContext(module.ll_context, 40)
  module.ll_int48 = llvm.intTypeInContext(module.ll_context, 48)
  module.ll_int56 = llvm.intTypeInContext(module.ll_context, 56)
  module.ll_float32 = llvm.floatTypeInContext(module.ll_context)
  module.ll_float64 = llvm.doubleTypeInContext(module.ll_context)
  module.ll_cstring = llvm.pointerType(module.ll_char, 0)
  module.ll_pointer = llvm.pointerType(module.ll_int8, 0)

  # nim string

  module.ll_nim_string = nil
  module.ll_generic_seq = nil
  module.ll_nim_type = nil

  # intrisics

  var args_memcpy32 = [module.ll_pointer, module.ll_pointer, module.ll_int32, module.ll_logic_bool]
  module.ll_memcpy32 = llvm.functionType(
    returnType = module.ll_void,
    paramTypes = addr args_memcpy32[0],
    paramCount = cuint len args_memcpy32,
    isVarArg = Bool 0)
  var args_memcpy64 = [module.ll_pointer, module.ll_pointer, module.ll_int64, module.ll_logic_bool]
  module.ll_memcpy64 = llvm.functionType(
    returnType = module.ll_void,
    paramTypes = addr args_memcpy64[0],
    paramCount = cuint len args_memcpy64,
    isVarArg = Bool 0)
  var args_memset32 = [module.ll_pointer, module.ll_int8, module.ll_int32, module.ll_logic_bool]
  module.ll_memset32 = llvm.functionType(
    returnType = module.ll_void,
    paramTypes = addr args_memset32[0],
    paramCount = cuint len args_memset32,
    isVarArg = Bool 0)
  var args_memset64 = [module.ll_pointer, module.ll_int8, module.ll_int64, module.ll_logic_bool]
  module.ll_memset64 = llvm.functionType(
    returnType = module.ll_void,
    paramTypes = addr args_memset64[0],
    paramCount = cuint len args_memset64,
    isVarArg = Bool 0)

proc setup_codegen(module: BModule) =
  let config = module.module_list.config

  block:
    module.ll_context = llvm.contextCreate()
    module.ll_builder = llvm.createBuilderInContext(module.ll_context)
    module.ll_module = llvm.moduleCreateWithNameInContext(module.module_sym.name.s, module.ll_context)

  block:
    var triple: string
    var cpu: cstring
    var reloc: RelocMode

    case config.target.targetCPU:
    of cpuI386:  cpu = "i686"
    of cpuAMD64: cpu = "x86-64"
    else: assert(false, "unsupported CPU")

    triple = select_target_triple(config.target)
    module.abi = select_target_abi(config.target)

    echo "target triple: ", triple
    echo "abi: ", module.abi

    var target: llvm.TargetRef = nil
    var err: cstring
    var features: cstring = ""
    if llvm.getTargetFromTriple(triple, addr target, addr err) != 0:
      echo "getTargetFromTriple error: ", err
      llvm.disposeMessage(err)
    var opt_level = llvm.CodeGenLevelNone
    reloc = RelocPIC#llvm.RelocDefault
    var model = llvm.CodeModelDefault
    module.ll_machine = llvm.createTargetMachine(target, triple, cpu, features, opt_level, reloc, model)
    let layout = llvm.createTargetDataLayout(module.ll_machine)
    llvm.setTarget(module.ll_module, triple)
    llvm.setModuleDataLayout(module.ll_module, layout)

  block:
    template get_attr(name, val): AttributeRef =
      let kind_id = llvm.getEnumAttributeKindForName(name, csize len name)
      llvm.createEnumAttribute(module.ll_context, kind_id, val)

    module.ll_sret = get_attr("sret", 0)
    module.ll_byval = get_attr("byval", 0)
    module.ll_zeroext = get_attr("zeroext", 0)
    module.ll_signext = get_attr("signext", 0)

# end setup_codegen

proc setup_init_proc(module: BModule) =
  module.init_proc = llvm.addFunction(
    module.ll_module,
    module.module_sym.name.s & "_module_main",
    llvm.functionType(module.ll_void, nil, 0, Bool 0))

  # module entry point
  let entry_bb = llvm.appendBasicBlockInContext(module.ll_context, module.init_proc, "module_entry")
  llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)
  module.top_scope = BScope(proc_val: module.init_proc, parent: nil)

proc finish_module*(module: BModule) =
  let incoming_bb = llvm.getInsertBlock(module.ll_builder)
  let exit_bb = llvm.appendBasicBlockInContext(module.ll_context, module.init_proc, "module_exit")
  llvm.positionBuilderAtEnd(module.ll_builder, incoming_bb)
  discard llvm.buildBr(module.ll_builder, exit_bb)

  llvm.positionBuilderAtEnd(module.ll_builder, exit_bb)
  discard llvm.buildRetVoid(module.ll_builder)

proc newModule*(module_list: BModuleList; module_sym: PSym; config: ConfigRef): BModule =
  new(result)
  result.module_sym = module_sym
  result.module_list = module_list
  result.type_cache = init_table[SigHash, TypeRef]()
  result.value_cache = init_table[int, ValueRef]()
  result.file_name = AbsoluteFile toFullPath(config, FileIndex module_sym.position)
  result.sig_collisions = initCountTable[SigHash]()
  setup_codegen(result)
  cache_types(result)
  setup_init_proc(result)

  if module_sym.position >= module_list.modules.len:
    set_len(module_list.modules, module_sym.position + 1)

  module_list.modules[module_sym.position] = result

proc find_module*(module: BModule; sym: PSym): BModule =
  let module_sym = getModule(sym)
  result = module.module_list.modules[module_sym.position]

proc gen_main_module*(module: BModule) =
  # emit app entry point procedure
  let entry_point = llvm.addFunction(
    module.ll_module,
    "main",
    llvm.functionType(module.ll_int32, nil, 0, Bool 0))
  let entry_bb = llvm.appendBasicBlockInContext(module.ll_context, entry_point, "app_entry")
  llvm.positionBuilderAtEnd(module.ll_builder, entry_bb)
  discard llvm.buildCall(module.ll_builder, module.init_proc, nil, 0, "")
  discard llvm.buildRet(module.ll_builder, llvm.constInt(module.ll_int32, culonglong 0, Bool 0))

# Scope Stack ------------------------------------------------------------------

proc open_scope*(module: BModule) =
  #echo "OPEN SCOPE"
  var scope = BScope()
  scope.parent = module.top_scope
  module.top_scope = scope

proc close_scope*(module: BModule) =
  #echo "CLOSE SCOPE"
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

proc mangle_global_name*(module: BModule; sym: PSym): string =
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
  assert typ != nil
  assert sym != nil
  echo "++ get_field_index:"
  echo "++ type.kind = ", typ.kind
  echo "++ sym.kind = ", sym.kind
  #let typ = skipTypes(typ, skipPtrs)
  let node = typ.n
  case node.kind:
  of nkSym:
    if sym == node.sym:
      result = 0
  of nkRecList:
    for son in node:
      if son.sym == sym:
        return
      else:
        inc result
  of nkRecCase:
    discard
  else:
    discard
  echo "++ result = ", result
