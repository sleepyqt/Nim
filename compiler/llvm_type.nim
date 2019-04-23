import ast, types
import llvm_data, llvm_abi
import llvm_dll as llvm
from sighashes import hashType

proc get_type*(module: BModule; typ: PType): TypeRef

# Array Type -------------------------------------------------------------------

proc get_array_type(module: BModule; typ: PType): TypeRef =
  let sig = hashType(typ)
  result = module.get_type(sig)
  if result == nil:
    let elem_type = get_type(module, elemType(typ))
    let elem_count = cuint lengthOrd(module.module_list.config, typ)
    result = llvm.arrayType(elem_type, elem_count)
    module.add_type(sig, result)

proc get_openarray_type(module: BModule; typ: PType): TypeRef =
  let sig = hashType(typ)
  result = module.get_type(sig)
  if result == nil:
    let name = "openArray"
    result = llvm.structCreateNamed(module.ll_context, name)
    var fields = [
      llvm.pointerType(get_type(module, typ.elemType), 0),
      module.ll_int]
    llvm.structSetBody(result, addr fields[0], cuint fields.len, 0)

    module.add_type(sig, result)

proc get_seq_type(module: BModule; typ: PType): TypeRef =
  let sig = hashType(typ)
  result = module.get_type(sig)
  if result == nil:
    let name = typ.sym.name.s
    result = llvm.structCreateNamed(module.ll_context, name)
    # todo
    module.add_type(sig, result)

# Object Type ------------------------------------------------------------------

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
    echo "gen_object_fields: case size: ", max_size, " align: ", max_align
    let elem_count =
      cuint (max_size div max_align)
    let elem_type =
      if max_align == 1: module.ll_int8
      elif max_align == 2: module.ll_int16
      elif max_align == 4: module.ll_int32
      elif max_align == 8: module.ll_int64
      else: module.ll_void
    fields.add llvm.arrayType(elem_type, elem_count)
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

    echo "+------------------------------------------------------+"
    echo " gen_object_type: ", name
    echo "+------------------------------------------------------+"
    echo " size: ", size, ", align: ", align

    var fields: seq[TypeRef]
    gen_object_fields(module, fields, typ.n)
    let fields_ptr = if fields.len == 0: nil else: addr fields[0]
    llvm.structSetBody(result, fields_ptr, cuint fields.len, 0)

    echo " result = ", result
    echo "+------------------------------------------------------+"

proc get_object_case_branch_type*(module: BModule; node: PNode): TypeRef =
  var fields: seq[TypeRef]
  gen_object_fields(module, fields, node)
  result = llvm.structCreateNamed(module.ll_context, "anonymous") # todo: cache?
  llvm.structSetBody(result, addr fields[0], cuint fields.len, 0)

# String Types -----------------------------------------------------------------

proc get_cstring_type*(module: BModule; typ: PType): TypeRef =
  module.ll_cstring

proc get_nim_string_type*(module: BModule; typ: PType): TypeRef =
  module.ll_nim_string

# Procedure Types --------------------------------------------------------------

proc get_proc_param_type*(module: BModule; typ: PType): TypeRef =
  if typ.kind == tyBool:
    result = module.ll_logic_bool
  else:
    result = get_type(module, typ)

proc get_proc_type*(module: BModule; typ: PType): TypeRef =
  var params = newSeq[TypeRef]()

  var state = abi_init_func_state(module)

  # return type
  var return_type: TypeRef = nil

  if typ[0] == nil:
    return_type = module.ll_void
  else:
    case abi_classify_return(module, typ[0], state):
    of ParamClass.Direct:
      return_type = get_proc_param_type(module, typ[0])
    of ParamClass.Indirect:
      return_type = module.ll_void
      params.add llvm.pointerType(get_proc_param_type(module, typ[0]), 0)
    of ParamClass.Extend:
      return_type = get_proc_param_type(module, typ[0])
    of ParamClass.Coerce1:
      var coerced = abi_coerce1(module, typ[0])
      return_type = coerced
    of ParamClass.Coerce2:
      var coerced = abi_coerce2(module, typ[0])
      return_type = llvm.structTypeInContext(module.ll_context, addr coerced[0], cuint 2, Bool false)

  # formal parameters types
  for param in typ.n.sons[1 .. ^1]:
    if isCompileTimeOnly(param.typ): continue
    case abi_classify(module, param.typ, state):
    of ParamClass.Direct:
      params.add get_proc_param_type(module, param.typ)
    of ParamClass.Indirect:
      params.add llvm.pointerType(get_proc_param_type(module, param.typ), 0)
    of ParamClass.Extend:
      params.add get_proc_param_type(module, param.typ)
    of ParamClass.Coerce1:
      let coerced = abi_coerce1(module, param.typ)
      params.add coerced
    of ParamClass.Coerce2:
      let coerced = abi_coerce2(module, param.typ)
      params.add coerced[0]
      params.add coerced[1]

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
  case int(getSize(module.module_list.config, typ)):
  of 1: result = module.ll_int8
  of 2: result = module.ll_int16
  of 4: result = module.ll_int32
  of 8: result = module.ll_int64
  else: assert(false, "unsupported yet")

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
  of tyBool: result = module.ll_bool
  of tyInt, tyUint: result = module.ll_int
  of tyInt8, tyUInt8: result = module.ll_int8
  of tyInt16, tyUint16: result = module.ll_int16
  of tyInt32, tyUInt32: result = module.ll_int32
  of tyInt64, tyUInt64: result = module.ll_int64
  of tyFloat, tyFloat64: result = module.ll_float64
  of tyFloat32: result = module.ll_float32
  of tyObject: result = get_object_type(module, typ)
  of tyTuple: result = get_object_type(module, typ)
  of tyPointer, tyNil: result = module.ll_pointer
  of tyProc: result = get_proc_type(module, typ)
  of tyRange: result = get_range_type(module, typ)
  of tyArray: result = get_array_type(module, typ)
  of tyEnum: result = get_enum_type(module, typ)
  of tyCString: result = module.ll_cstring
  of tySet: result = get_set_type(module, typ)
  of tyPtr, tyRef, tyVar, tyLent: result = get_ptr_type(module, typ)
  of tyChar: result = module.ll_char
  of tyString: result = module.ll_nim_string
  of tySequence: result = get_seq_type(module, typ)
  of tyDistinct: result = get_type(module, typ.lastSon)
  of tyOpenArray: result = get_openarray_type(module, typ)
  else: echo "get_type: unknown type kind: ", typ.kind
  if result == nil:
    echo "get_type fail: ", typ.kind
  #echo ">>>>> mapped type: ", typ.kind, " ----> ", llvm.getTypeKind(result)

