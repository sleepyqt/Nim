import ast, types
import llvm_data
import llvm_dll as llvm

proc get_type*(module: BModule; typ: PType): TypeRef

# Composite Types --------------------------------------------------------------

proc get_array_type*(module: BModule; typ: PType): TypeRef =
  discard

proc get_object_type*(module: BModule; typ: PType): TypeRef =
  let name = typ.sym.name.s
  result = llvm.structCreateNamed(module.ll_context, name)

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

# Procedure Types --------------------------------------------------------------

proc get_proc_type*(module: BModule; typ: PType): TypeRef =
  # return type
  #let return_type = if typ.sons[0] == nil: module.vm_void
  #                  else: get_type(module, typ.sons[0])
  let return_type = module.ll_void
  # param type
  var param = newSeq[TypeRef]()
  for i in countup(1, sonsLen(typ.n) - 1):
    var param = typ.n.sons[i].sym
    if isCompileTimeOnly(param.typ): continue
    #let param_type = get_type(module, param)
  result = llvm.functionType(
    returnType = return_type,
    paramTypes = if param.len == 0: nil else: addr(param[0]),
    paramCount = cuint(param.len),
    isVarArg = Bool(0))

# ------------------------------------------------------------------------------

proc get_enum_type(module: BModule; typ: PType): TypeRef =
  discard

proc get_range_type(module: BModule; typ: PType): TypeRef =
  discard

proc get_type*(module: BModule; typ: PType): TypeRef =
  ## maps nim type LLVM type
  assert module != nil
  assert typ != nil
  case typ.kind:
  of tyInt, tyUint: result = module.ll_int
  of tyInt8, tyUInt8: result = module.ll_int8
  of tyInt16, tyUint16: result = module.ll_int16
  of tyInt32, tyUInt32: result = module.ll_int32
  of tyInt64, tyUInt64: result = module.ll_int64
  of tyFloat, tyFloat64: result = module.ll_float64
  of tyFloat32: result = module.ll_float32
  of tyObject: result = get_object_type(module, typ)
  of tyTuple: result = get_object_type(module, typ)
  of tyPointer, tyNil: result = module.ll_void
  of tyProc: result = get_proc_type(module, typ)
  of tyRange: result = get_range_type(module, typ)
  else: echo "get_type: unknown type kind: ", typ.kind
