import ast, types
import llvm_data
import llvm_dll as llvm
from sighashes import hashType

proc get_type*(module: BModule; typ: PType): TypeRef

# ------------------------------------------------------------------------------

proc is_signed_type*(typ: PType): bool =
  typ.kind in {tyInt .. tyInt64}

proc get_type_size*(module: BModule; typ: PType): BiggestInt =
  result = getSize(module.module_list.config, typ)

proc get_type_align*(module: BModule; typ: PType): BiggestInt =
  discard

# Composite Types --------------------------------------------------------------

proc get_array_type(module: BModule; typ: PType): TypeRef =
  let sig = hashType(typ)
  result = module.get_type(sig)
  if result == nil:
    let elem_type = get_type(module, elemType(typ))
    let elem_count = cuint lengthOrd(module.module_list.config, typ)
    result = llvm.arrayType(elem_type, elem_count)
    module.add_type(sig, result)

proc get_object_type(module: BModule; typ: PType): TypeRef =
  let sig = hashType(typ)
  result = module.get_type(sig)
  if result == nil:
    let name = typ.sym.name.s
    result = llvm.structCreateNamed(module.ll_context, name)
    module.add_type(sig, result)

    var fields: seq[TypeRef]

    proc gen_fields(node: PNode) =
      case node.kind:
      of nkRecList: (for son in node: gen_fields(son))
      of nkSym: (if node.sym.typ.kind != tyEmpty: fields.add get_type(module, node.sym.typ))
      else: discard

    gen_fields(typ.n)
    let fields_ptr = if fields.len == 0: nil else: addr fields[0]
    llvm.structSetBody(result, fields_ptr, cuint fields.len, 0)

# String Types -----------------------------------------------------------------

proc get_cstring_type*(module: BModule; typ: PType): TypeRef =
  module.ll_cstring

proc get_nim_string_type*(module: BModule; typ: PType): TypeRef =
  module.ll_nim_string

proc get_seq_type(module: BModule; typ: PType): TypeRef =
  discard

# Procedure Types --------------------------------------------------------------

const CompositeTypes* = {tyObject, tyArray}

proc get_proc_type*(module: BModule; typ: PType): TypeRef =
  var params = newSeq[TypeRef]()
  # return type
  var return_type: TypeRef = nil
  if typ[0] == nil:
    return_type = module.ll_void
  else:
    if typ[0].kind in CompositeTypes:
      # composite types returned in first argument
      params.add llvm.pointerType(get_type(module, typ[0]), 0)
      return_type = module.ll_void
    elif typ[0].kind == tyBool:
      return_type = module.ll_logic_bool
    else:
      return_type = get_type(module, typ[0])

  # param type
  for i in countup(1, sonsLen(typ.n) - 1):
    let param = typ.n.sons[i].sym
    if not isCompileTimeOnly(param.typ):
      let param_type = param.typ
      let param_ll_type =
        if param_type.kind in CompositeTypes:
          # composite types passed as pointers
          llvm.pointerType(get_type(module, param_type), 0)
        elif param_type.kind == tyBool:
          # bool types passed as i1
          module.ll_logic_bool
        else:
          get_type(module, param_type)
      params.add(param_ll_type)
  result = llvm.functionType(
    returnType = return_type,
    paramTypes = if params.len == 0: nil else: addr(params[0]),
    paramCount = cuint(params.len),
    isVarArg = Bool(0))

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
  else: echo "get_type: unknown type kind: ", typ.kind
  if result == nil:
    echo "get_type fail: ", typ.kind
  echo ">>>>> mapped type: ", typ.kind, " ----> ", llvm.getTypeKind(result)
