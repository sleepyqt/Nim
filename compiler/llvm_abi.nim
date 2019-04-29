import ast, types
import llvm_data
import llvm_dll as llvm

# ------------------------------------------------------------------------------

type ParamClass* = enum
  Direct # Passed directly in register
  Indirect # Passed indirectly with pointer
  Extend # Passed directly and sign-zero extended
  Coerce1 # coerced into register
  Coerce2 # coerced into two registers

type FuncState* = object
  int_regs*: int
  flt_regs*: int

proc abi_init_func_state*(module: BModule; abi: PlatformABI): FuncState
proc abi_coerce1*(module: BModule; abi: PlatformABI; typ: PType): TypeRef
proc abi_coerce2*(module: BModule; abi: PlatformABI; typ: PType): array[2, TypeRef]
proc abi_classify*(module: BModule; abi: PlatformABI; typ: PType; state: var FuncState): ParamClass
proc abi_classify_return*(module: BModule; abi: PlatformABI; typ: PType; state: FuncState): ParamClass

# - Generic --------------------------------------------------------------------

proc abi_init_func_state_generic(module: BModule): FuncState =
  result.int_regs = 0
  result.flt_regs = 0

proc abi_coerce1_generic(module: BModule; typ: PType): TypeRef =
  discard

proc abi_coerce2_generic(module: BModule; typ: PType): array[2, TypeRef] =
  discard

proc abi_classify_generic(module: BModule; typ: PType; state: var FuncState): ParamClass =
  result = ParamClass.Direct

proc abi_classify_return_generic(module: BModule; typ: PType; state: FuncState): ParamClass =
  result = ParamClass.Direct

# - AMD64_SystemV --------------------------------------------------------------

proc abi_init_func_state_amd64_systemv(module: BModule): FuncState =
  result.int_regs = 6 # rdi, rsi, rdx, rcx, r8, r9
  result.flt_regs = 8 # xmm0 .. xmm7

proc abi_coerce1_amd64_systemv(module: BModule; typ: PType): TypeRef =
  case get_type_size(module, typ):
  of 1:  result = module.ll_int8
  of 2:  result = module.ll_int16
  of 3:  result = module.ll_int24
  of 4:  result = module.ll_int32
  of 5:  result = module.ll_int40
  of 6:  result = module.ll_int48
  of 7:  result = module.ll_int56
  of 8:  result = module.ll_int64
  else: assert false

proc abi_coerce2_amd64_systemv(module: BModule; typ: PType): array[2, TypeRef] =
  if typ.kind in {tyOpenArray}:
    result[0] = module.ll_int64
    result[1] = module.ll_int64
  elif typ.kind in {tyTuple, tyObject, tyArray}:
    case get_type_size(module, typ):
    of 9:  result[0] = module.ll_int64 ; result[1] = module.ll_int8
    of 10: result[0] = module.ll_int64 ; result[1] = module.ll_int16
    of 11: result[0] = module.ll_int64 ; result[1] = module.ll_int24
    of 12: result[0] = module.ll_int64 ; result[1] = module.ll_int32
    of 13: result[0] = module.ll_int64 ; result[1] = module.ll_int40
    of 14: result[0] = module.ll_int64 ; result[1] = module.ll_int48
    of 15: result[0] = module.ll_int64 ; result[1] = module.ll_int56
    of 16: result[0] = module.ll_int64 ; result[1] = module.ll_int64
    else: assert false
  else:
    assert false

proc abi_classify_amd64_systemv(module: BModule; typ: PType; state: var FuncState): ParamClass =
  if typ.kind in {tyBool}:
    result = ParamClass.Extend
  elif typ.kind in {tyInt .. tyInt64, tyUint .. tyUInt64, tyChar, tyEnum, tyPtr, tyPointer}:
    result = ParamClass.Direct
    dec state.int_regs
  elif typ.kind in {tyFloat, tyFloat32, tyFloat64}:
    result = ParamClass.Direct
    dec state.flt_regs
  elif typ.kind in {tyOpenArray}:
    if (state.int_regs - 2 >= 0):
      result = ParamClass.Coerce2
    else:
      result = ParamClass.Indirect
  elif typ.kind in {tyArray, tyObject, tyTuple}:
    var flt_regs = 0
    var int_regs = 0
    let size = get_type_size(module, typ)
    if size <= 8:
      let coerced = abi_coerce1_amd64_systemv(module, typ)
      let kind = llvm.getTypeKind(coerced)
      if kind == IntegerTypeKind: inc int_regs
      if kind == FloatTypeKind: inc flt_regs
      if (state.int_regs - int_regs >= 0) and (state.flt_regs - flt_regs >= 0):
        result = ParamClass.Coerce1
        dec state.int_regs, int_regs
        dec state.flt_regs, flt_regs
      else:
        result = ParamClass.Indirect
    elif size <= 16:
      let coerced = abi_coerce2_amd64_systemv(module, typ)
      let kind1 = llvm.getTypeKind(coerced[0])
      let kind2 = llvm.getTypeKind(coerced[1])
      if kind1 == IntegerTypeKind: inc int_regs
      if kind2 == IntegerTypeKind: inc int_regs
      if kind1 == FloatTypeKind: inc flt_regs
      if kind2 == FloatTypeKind: inc flt_regs
      if (state.int_regs - int_regs >= 0) and (state.flt_regs - flt_regs >= 0):
        result = ParamClass.Coerce2
        dec state.int_regs, int_regs
        dec state.flt_regs, flt_regs
      else:
        result = ParamClass.Indirect
    else:
      result = ParamClass.Indirect
  elif typ.kind in {tyVar}:
    result = ParamClass.Indirect
  else:
    assert false

proc abi_classify_return_amd64_systemv(module: BModule; typ: PType; state: FuncState): ParamClass =
  if typ.kind in {tyBool}:
    result = ParamClass.Extend
  elif typ.kind in {tyInt .. tyInt64}:
    result = ParamClass.Direct
  elif typ.kind in {tyArray, tyObject, tyTuple}:
    let size = get_type_size(module, typ)
    if size <= 8:
      result = ParamClass.Coerce1
    elif size <= 16:
      result = ParamClass.Coerce2
    else:
      result = ParamClass.Indirect
  else:
    assert false

# - AMD64_Windows --------------------------------------------------------------

proc abi_init_func_state_amd64_windows(module: BModule): FuncState =
  result.int_regs = 0
  result.flt_regs = 0

proc abi_coerce1_amd64_windows(module: BModule; typ: PType): TypeRef =
  case get_type_size(module, typ):
  of 1:  result = module.ll_int8
  of 2:  result = module.ll_int16
  of 4:  result = module.ll_int32
  of 8:  result = module.ll_int64
  else: assert false

proc abi_coerce2_amd64_windows(module: BModule; typ: PType): array[2, TypeRef] =
  discard

proc abi_classify_amd64_windows(module: BModule; typ: PType; state: var FuncState): ParamClass =
  if typ.kind in {tyObject, tyTuple}:
    if get_type_size(module, typ).int in [1, 2, 4, 8]:
      result = ParamClass.Coerce1
    else:
      result = ParamClass.Indirect
  elif typ.kind in {tyArray}:
    result = ParamClass.Indirect
  elif typ.kind in {tyVar}:
    result = ParamClass.Direct
  elif typ.kind in {tyOpenArray}:
    result = ParamClass.Indirect
  else:
    result = ParamClass.Direct

proc abi_classify_return_amd64_windows(module: BModule; typ: PType; state: FuncState): ParamClass =
  if typ.kind in {tyObject, tyTuple}:
    if get_type_size(module, typ).int in [1, 2, 4, 8]:
      result = ParamClass.Coerce1
    else:
      result = ParamClass.Indirect
  elif typ.kind in {tyArray}:
    result = ParamClass.Indirect
  else:
    result = ParamClass.Direct

# ------------------------------------------------------------------------------

proc abi_init_func_state*(module: BModule; abi: PlatformABI): FuncState =
  case abi:
  of PlatformABI.Generic:
    result = abi_init_func_state_generic(module)
  of PlatformABI.AMD64_SystemV:
    result = abi_init_func_state_amd64_systemv(module)
  of PlatformABI.AMD64_Windows:
    result = abi_init_func_state_amd64_windows(module)

proc abi_coerce1*(module: BModule; abi: PlatformABI; typ: PType): TypeRef =
  case abi:
  of PlatformABI.Generic:
    result = abi_coerce1_generic(module, typ)
  of PlatformABI.AMD64_SystemV:
    result = abi_coerce1_amd64_systemv(module, typ)
  of PlatformABI.AMD64_Windows:
    result = abi_coerce1_amd64_windows(module, typ)

proc abi_coerce2*(module: BModule; abi: PlatformABI; typ: PType): array[2, TypeRef] =
  case abi:
  of PlatformABI.Generic:
    result = abi_coerce2_generic(module, typ)
  of PlatformABI.AMD64_SystemV:
    result = abi_coerce2_amd64_systemv(module, typ)
  of PlatformABI.AMD64_Windows:
    result = abi_coerce2_amd64_windows(module, typ)

proc abi_classify*(module: BModule; abi: PlatformABI; typ: PType; state: var FuncState): ParamClass =
  # classify function parameter
  case abi:
  of PlatformABI.Generic:
    result = abi_classify_generic(module, typ, state)
  of PlatformABI.AMD64_SystemV:
    result = abi_classify_amd64_systemv(module, typ, state)
  of PlatformABI.AMD64_Windows:
    result = abi_classify_amd64_windows(module, typ, state)

proc abi_classify_return*(module: BModule; abi: PlatformABI; typ: PType; state: FuncState): ParamClass =
  # classify function return parameter
  case abi:
  of PlatformABI.Generic:
    result = abi_classify_return_generic(module, typ, state)
  of PlatformABI.AMD64_SystemV:
    result = abi_classify_return_amd64_systemv(module, typ, state)
  of PlatformABI.AMD64_Windows:
    result = abi_classify_return_amd64_windows(module, typ, state)
