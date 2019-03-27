import ast, types
import llvm_data, llvm_dll

# Composite Types --------------------------------------------------------------

proc get_array_type*(module: BModule; typ: PType): TypeRef =
  discard

proc get_object_type*(module: BModule; typ: PType): TypeRef =
  discard

# String Types -----------------------------------------------------------------

proc get_cstring_type*(module: BModule; typ: PType): TypeRef =
  discard

proc get_nim_string_type*(module: BModule; typ: PType): TypeRef =
  discard

# ------------------------------------------------------------------------------

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
  else: echo "get_type: unknown type kind: ", typ.kind
