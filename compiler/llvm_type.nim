import ast, types
import llvm_data
import llvm_dll as llvm
from sighashes import hashType
from magicsys import getCompilerProc
from astalgo import debug

proc get_type*(module: BModule; typ: PType): TypeRef

# ------------------------------------------------------------------------------

proc is_signed_type*(typ: PType): bool =
  typ.kind in {tyInt .. tyInt64}

proc get_type_size*(module: BModule; typ: PType): BiggestInt =
  result = getSize(module.module_list.config, typ)

proc get_type_align*(module: BModule; typ: PType): BiggestInt =
  result = getAlign(module.module_list.config, typ)

proc type_to_ptr*(value: TypeRef): TypeRef =
  result = llvm.pointerType(value, 0)

# ------------------------------------------------------------------------------

proc live_as_pointer*(module: BModule; typ: PType): bool =
  result = typ.kind in {tyObject, tyArray, tyTuple, tyOpenArray}

proc get_runtime_type*(module: BModule; name: string): TypeRef =
  let sym = getCompilerProc(module.module_list.graph, name)
  if sym == nil: module.ice(name & " missing")
  result = get_type(module, sym.typ)

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
    else: assert false, $size
  of 4:
    case size:
    of 1 .. 4:   result = @[module.ll_int32]
    of 5 .. 8:   result = @[module.ll_int32, module.ll_int32]
    else: assert false, $size
  else:
    assert false, $size

# ------------------------------------------------------------------------------

import llvm_abi

# Array Type -------------------------------------------------------------------

proc get_generic_seq_type*(module: BModule): TypeRef =
  result = module.ll_generic_seq
  if result == nil:
    module.ll_generic_seq = get_runtime_type(module, "TGenericSeq")
    result = module.ll_generic_seq

proc get_array_type(module: BModule; typ: PType): TypeRef =
  let sig = hashType(typ)
  result = module.get_type(sig)
  if result == nil:
    let elem_type = get_type(module, elemType(typ))
    let elem_count = cuint lengthOrd(module.module_list.config, typ)
    result = llvm.arrayType(elem_type, elem_count)
    module.add_type(sig, result)

proc get_openarray_type(module: BModule; typ: PType): TypeRef =
  result = type_to_ptr get_type(module, typ.elemType)

proc get_seq_type(module: BModule; typ: PType): TypeRef =
  let sig = hashType(typ)
  result = module.get_type(sig)
  if result == nil:
    #let name = typ.sym.name.s
    #debug typ
    let struct = llvm.structCreateNamed(module.ll_context, "seq")

    let header = get_generic_seq_type(module)
    let elem_type = llvm.arrayType(get_type(module, elemType(typ)), 0)

    var fields = [ header, elem_type ]

    llvm.structSetBody(struct, addr fields[0], cuint fields.len, 0)

    result = type_to_ptr struct

    module.add_type(sig, result)

proc get_unchecked_array_type(module: BModule; typ: PType): TypeRef =
  let elem_type = get_type(module, elemType(typ))
  let elem_count = cuint 0
  result = llvm.arrayType(elem_type, elem_count)

# Object Type ------------------------------------------------------------------

proc get_nim_type(module: BModule): TypeRef =
  result = module.ll_nim_type
  if result == nil:
    module.ll_nim_type = get_runtime_type(module, "TNimType")
    result = module.ll_nim_type

proc rec_list_size_align(module: BModule; node: PNode): tuple[size, align: BiggestInt] =
  case node.kind:
  of nkRecList:
    for item in node:
      result.align = max(result.align, get_type_align(module, item.sym.typ))
    for item in node:
      result.size = result.size + max(result.align, get_type_size(module, item.sym.typ))
  of nkSym:
    result.align = get_type_align(module, node.sym.typ)
    result.size = get_type_size(module, node.sym.typ)
  else:
    assert false
  # todo: no support for nested case objects yet

proc gen_object_fields(module: BModule; fields: var seq[TypeRef]; node: PNode) =
  case node.kind:
  of nkRecList:
    # list of fields
    for item in node:
      gen_object_fields(module, fields, item)
  of nkSym:
    # single field
    if node.sym.typ.kind != tyEmpty:
      fields.add get_type(module, node.sym.typ)
  of nkRecCase:
    # case object
    let discriminant = node[0]
    fields.add get_type(module, discriminant.typ)
    # find biggest branch and replace it with array of the same size and alignment
    var max_size, max_align: BiggestInt
    for branch in node.sons[1 .. ^1]:
      if branch.kind in {nkOfBranch, nkElse}:
        let (size, align) = rec_list_size_align(module, branch.lastSon)
        max_size = max(max_size, size)
        max_align = max(max_align, align)
    #echo "gen_object_fields: case size: ", max_size, " align: ", max_align
    let elem_type =
      if max_align == 1: module.ll_int8
      elif max_align == 2: module.ll_int16
      elif max_align == 4: module.ll_int32
      elif max_align == 8: module.ll_int64
      else: module.ll_void
    if max_align != 0 and max_size != 0:
      fields.add llvm.arrayType(elem_type, cuint (max_size div max_align))
  else:
    discard

proc get_object_type(module: BModule; typ: PType): TypeRef =
  let sig = hashType(typ)
  result = module.get_type(sig)
  if result == nil:
    let name = typ.sym.name.s
    result = llvm.structCreateNamed(module.ll_context, name)
    module.add_type(sig, result)

    let size = get_type_size(module, typ)
    let align = get_type_align(module, typ)
    let super = if typ.sons.len == 0: nil else: typ[0]

    var fields: seq[TypeRef]
    if super == nil:
      if (typ.sym != nil and sfPure in typ.sym.flags) or (tfFinal in typ.flags):
        discard
      else:
        fields.add type_to_ptr get_nim_type(module)
    else:
      fields.add get_object_type(module, super)

    gen_object_fields(module, fields, typ.n)

    if len(fields) == 0:
      fields.add module.ll_int8

    let fields_ptr = if fields.len == 0: nil else: addr fields[0]
    llvm.structSetBody(result, fields_ptr, cuint fields.len, 0)

proc get_object_case_branch_type*(module: BModule; node: PNode): TypeRef =
  var fields: seq[TypeRef]
  gen_object_fields(module, fields, node)
  result = llvm.structCreateNamed(module.ll_context, "anonymous") # todo: cache?
  llvm.structSetBody(result, addr fields[0], cuint fields.len, 0)

proc get_tuple_type(module: BModule; typ: PType): TypeRef =
  var fields: seq[TypeRef]
  for item in typ.sons:
    fields.add get_type(module, item)
  result = llvm.structTypeInContext(module.ll_context, addr fields[0], cuint len fields, Bool 0)

# String Types -----------------------------------------------------------------

proc get_cstring_type*(module: BModule; typ: PType): TypeRef =
  module.ll_cstring

proc get_nim_string_type*(module: BModule): TypeRef =
  result = module.ll_nim_string
  if result == nil:
    module.ll_nim_string = type_to_ptr get_runtime_type(module, "NimStringDesc")
    result = module.ll_nim_string

# Procedure Types --------------------------------------------------------------

proc get_proc_param_type*(module: BModule; typ: PType): TypeRef =
  if typ.kind == tyBool:
    result = module.ll_bool
  else:
    result = get_type(module, typ)

proc get_proc_type*(module: BModule; typ: PType): TypeRef =
  var params = newSeq[TypeRef]()

  var abi = get_abi(module)

  # return type
  var return_type: TypeRef = nil

  if typ[0] == nil:
    return_type = module.ll_void
  else:
    let return_type_info = abi.classify_return_type(module, typ[0])

    case return_type_info.class:

    of ArgClass.Direct:
      return_type = get_proc_param_type(module, typ[0])

    of ArgClass.Indirect:
      return_type = module.ll_void
      params.add type_to_ptr get_proc_param_type(module, typ[0])

    of ArgClass.Expand:
      let ll_type = get_proc_param_type(module, typ[0])
      var expanded = expand_struct_to_words(module, typ[0])
      return_type = llvm.structTypeInContext(module.ll_context, addr expanded[0], cuint len expanded, Bool false)

    of ArgClass.Ignore:
      discard

    of ArgClass.OpenArray:
      discard

  # formal parameters types
  for param in typ.n.sons[1 .. ^1]:
    if isCompileTimeOnly(param.typ): continue
    let param_type_info = abi.classify_argument_type(module, param.typ)

    case param_type_info.class:

    of ArgClass.Direct:
      params.add get_proc_param_type(module, param.typ)

    of ArgClass.Indirect:
      params.add type_to_ptr get_proc_param_type(module, param.typ)

    of ArgClass.Expand:
      if ExpandToWords in param_type_info.flags:
        let expanded = expand_struct_to_words(module, param.typ)
        for field in expanded: params.add field
      else:
        let expanded = expand_struct(module, get_proc_param_type(module, param.typ))
        for field in expanded: params.add field

    of ArgClass.Ignore:
      discard

    of ArgClass.OpenArray:
      params.add get_proc_param_type(module, param.typ) # ptr T
      params.add module.ll_int # length

  assert return_type != nil

  result = llvm.functionType(
    returnType = return_type,
    paramTypes = if params.len == 0: nil else: addr(params[0]),
    paramCount = cuint(params.len),
    isVarArg = if tfVarargs in typ.flags: Bool(1) else: Bool(0))

# Scalar Types -----------------------------------------------------------------

proc get_enum_type(module: BModule; typ: PType): TypeRef =
  if firstOrd(module.module_list.config, typ) < 0:
    result = module.ll_int32
  else:
    case int(getSize(module.module_list.config, typ)):
    of 1: result = module.ll_int8
    of 2: result = module.ll_int16
    of 4: result = module.ll_int32
    of 8: result = module.ll_int64
    else: assert false

proc get_range_type(module: BModule; typ: PType): TypeRef =
  result = get_type(module, typ[0])

proc get_set_type(module: BModule; typ: PType): TypeRef =
  let size = int(getSize(module.module_list.config, typ))
  case size:
  of 1: result = module.ll_int8
  of 2: result = module.ll_int16
  of 4: result = module.ll_int32
  of 8: result = module.ll_int64
  else: result = llvm.arrayType(module.ll_int8, cuint size)

# ------------------------------------------------------------------------------

proc get_ptr_type(module: BModule; typ: PType): TypeRef =
  let elem_type = get_type(module, elemType(typ))
  result = llvm.pointerType(elem_type, 0)

# ------------------------------------------------------------------------------

proc get_type*(module: BModule; typ: PType): TypeRef =
  ## maps nim type LLVM type
  assert module != nil
  assert typ != nil
  case typ.kind:
  of tyDistinct, tyTypeDesc: result = get_type(module, typ.lastSon)
  of tyBool: result = module.ll_mem_bool
  of tyInt, tyUint: result = module.ll_int
  of tyInt8, tyUInt8: result = module.ll_int8
  of tyInt16, tyUint16: result = module.ll_int16
  of tyInt32, tyUInt32: result = module.ll_int32
  of tyInt64, tyUInt64: result = module.ll_int64
  of tyFloat, tyFloat64: result = module.ll_float64
  of tyFloat32: result = module.ll_float32
  of tyObject: result = get_object_type(module, typ)
  of tyTuple: result = get_tuple_type(module, typ)
  of tyPointer, tyNil: result = module.ll_pointer
  of tyProc: result = type_to_ptr get_proc_type(module, typ)
  of tyRange: result = get_range_type(module, typ)
  of tyArray: result = get_array_type(module, typ)
  of tyEnum: result = get_enum_type(module, typ)
  of tyCString: result = module.ll_cstring
  of tySet: result = get_set_type(module, typ)
  of tyPtr, tyRef: result = get_ptr_type(module, typ)
  of tyVar: result = llvm.pointerType(get_type(module, elemType(typ)), 0)
  of tyChar: result = module.ll_char
  of tyString: result = get_nim_string_type(module)
  of tySequence: result = get_seq_type(module, typ)
  of tyOpenArray, tyVarargs: result = get_openarray_type(module, typ)
  of tyUncheckedArray: result = get_unchecked_array_type(module, typ)
  else: echo "get_type: unknown type kind: ", typ.kind

  if result == nil: echo "get_type fail: ", typ.kind
  #echo ">>>>> mapped type: ", typ.kind, " ----> ", llvm.getTypeKind(result)

# ------------------------------------------------------------------------------

proc gen_type_info*(module: BModule; typ: PType): ValueRef =
  let sig = hashType(typ)
  result = module.get_type_info(sig)
  if result == nil:
    echo "☭☭ --------------------------------------------------"
    echo "☭☭ gen_type_info:   ", typ.kind
    echo "☭☭ --------------------------------------------------"
    let nim_type = get_nim_type(module)
    #echo "TNimType: ", nim_type

    let initializer = llvm.constNull(nim_type)

    result = llvm.addGlobal(module.ll_module, nim_type, "rtti")

    llvm.setInitializer(result, initializer)
    llvm.setGlobalConstant(result, Bool 1)
    llvm.setLinkage(result, PrivateLinkage)

    module.add_type_info(sig, result)
