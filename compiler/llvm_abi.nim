# included from "llvm_pass.nim"

type ArgClass = enum
  Direct # Passed directly in register
  Indirect # Passed indirectly via a pointer
  Expand # object or tuple expandend into consecutive arguments
  Ignore
  OpenArray

type ArgFlag = enum
  ByVal, InReg, Sext, Zext, ExpandToWords

type ArgInfo = object
  class: ArgClass
  flags: set[ArgFlag]

type FuncFlag = enum
  AttrUwtable

type FuncInfo = object
  flags: set[FuncFlag]

# ------------------------------------------------------------------------------

type BaseAbi = ref object of RootObj

method classify_argument_type(abi: BaseAbi; module: BModule; typ: PType): ArgInfo {.base.} =
  assert false

method classify_return_type(abi: BaseAbi; module: BModule; typ: PType): ArgInfo {.base.} =
  assert false

method get_func_info(abi: BaseAbi): FuncInfo {.base.} =
  assert false

proc map_call_conv(module: BModule; cc: TCallingConvention): llvm.CallConv

proc get_abi(module: BModule): BaseAbi

# ------------------------------------------------------------------------------

type GenericAbi = ref object of BaseAbi

method classify_argument_type(abi: GenericAbi; module: BModule; typ: PType): ArgInfo =
  case typ.kind:

  of tyArray:
    result.class = Indirect

  of tyObject, tyTuple:
    result.class = Expand
    result.flags.incl ExpandToWords

  of tyInt .. tyInt64, tyUint .. tyUInt64, tyFloat32, tyFloat64, tyFloat:
    result.class = Direct

  of tyCString, tyPtr, tyPointer, tyVar:
    result.class = Direct

  of tyBool:
    result.class = Direct
    result.flags.incl Zext

  of tyOpenArray, tyVarargs:
    result.class = OpenArray

  else:
    assert false

method classify_return_type(abi: GenericAbi; module: BModule; typ: PType): ArgInfo =
  case typ.kind:

  of tyArray:
    result.class = Indirect

  of tyObject, tyTuple:
    result.class = Indirect

  of tyInt .. tyInt64, tyUint .. tyUInt64, tyFloat32, tyFloat64, tyFloat:
    result.class = Direct

  of tyBool:
    result.class = Direct
    result.flags.incl Zext

  else:
    assert false

method get_func_info(abi: GenericAbi): FuncInfo =
  discard

# ------------------------------------------------------------------------------

type Amd64AbiSystemV = ref object of BaseAbi
  sse_regs: int
  int_regs: int

method classify_argument_type(abi: Amd64AbiSystemV; module: BModule; typ: PType): ArgInfo =
  case typ.kind:

  of tyArray:
    result.class = Indirect

  of tyObject, tyTuple:
    let size = get_type_size(module, typ)
    if size in 1 .. 16:
      result.class = Expand
      result.flags.incl ExpandToWords
    else:
      result.class = Indirect
      result.flags.incl ByVal

  of tyInt .. tyInt64, tyUint .. tyUInt64:
    result.class = Direct

  of tyFloat32, tyFloat64, tyFloat:
    result.class = Direct

  of tyEnum:
    result.class = Direct

  of tyChar:
    result.class = Direct

  of tyCString, tyPtr, tyPointer, tyVar, tyRef, tyString, tySequence:
    result.class = Direct

  of tyBool:
    result.class = Direct
    result.flags.incl Zext

  of tyOpenArray, tyVarargs:
    result.class = OpenArray

  of tySet:
    let size = get_type_size(module, typ)
    case size:
    of 1, 2, 4, 8:
      result.class = Direct
    else:
      result.class = Indirect

  of tyProc:
    result.class = Direct

  of tyRange:
    result.class = Direct

  of tyTypeDesc:
    result.class = Ignore

  else:
    module.ice("classify_argument_type: " & $typ.kind)

method classify_return_type(abi: Amd64AbiSystemV; module: BModule; typ: PType): ArgInfo =
  case typ.kind:

  of tyArray:
    result.class = Indirect

  of tyObject, tyTuple:
    result.class = Indirect

  of tyInt .. tyInt64, tyUint .. tyUInt64:
    result.class = Direct

  of tyFloat32, tyFloat64, tyFloat:
    result.class = Direct

  of tyEnum:
    result.class = Direct

  of tyChar:
    result.class = Direct

  of tyCString, tyPtr, tyPointer, tyVar, tyRef, tyString, tySequence:
    result.class = Direct

  of tyBool:
    result.class = Direct
    result.flags.incl Zext

  of tyOpenArray, tyVarargs:
    result.class = OpenArray

  of tyProc:
    result.class = Direct

  of tyRange:
    result.class = Direct

  else:
    module.ice("classify_return_type: " & $typ.kind)

method get_func_info(abi: Amd64AbiSystemV): FuncInfo =
  result.flags = {AttrUwtable}

# ------------------------------------------------------------------------------

type Amd64AbiWindows = ref object of BaseAbi

method classify_argument_type(abi: Amd64AbiWindows; module: BModule; typ: PType): ArgInfo =
  case typ.kind:

  of tyArray:
    result.class = ArgClass.Indirect

  of tyObject, tyTuple:
    let size = get_type_size(module, typ)
    if size.int in [1, 2, 4, 8]:
      result.class = ArgClass.Expand
      result.flags.incl(ExpandToWords)
    else:
      result.class = ArgClass.Indirect

  of tyInt .. tyInt64, tyUint .. tyUInt64, tyFloat32, tyFloat64, tyFloat:
    result.class = ArgClass.Direct

  of tyPtr, tyPointer, tyVar, tyCString:
    result.class = ArgClass.Direct

  else:
    module.ice("classify_argument_type: " & $typ.kind)

method classify_return_type(abi: Amd64AbiWindows; module: BModule; typ: PType): ArgInfo =
  case typ.kind:

  of tyArray:
    result.class = ArgClass.Indirect

  of tyObject, tyTuple:
    result.class = ArgClass.Indirect

  of tyInt .. tyInt64, tyUint .. tyUInt64, tyFloat32, tyFloat64, tyFloat:
    result.class = ArgClass.Direct

  of tyPtr, tyPointer, tyVar, tyCString:
    result.class = ArgClass.Direct

  else:
    module.ice("classify_return_type: " & $typ.kind)

method get_func_info(abi: Amd64AbiWindows): FuncInfo =
  discard

# ------------------------------------------------------------------------------

type X86Abi = ref object of BaseAbi

proc x86_can_expand(module: BModule; struct: PType): bool =
  let ll_type = get_type(module, struct)

  let expandend = expand_struct(module, ll_type)
  for item in expandend:
    let kind = llvm.getTypeKind(item)
    if kind != IntegerTypeKind: return false
    if llvm.getIntTypeWidth(item) notin [cuint 32, 64]: return false

  result = true

method classify_argument_type(abi: X86Abi; module: BModule; typ: PType): ArgInfo =
  case typ.kind:

  of tyArray:
    result.class = ArgClass.Indirect

  of tyObject, tyTuple:
    if x86_can_expand(module, typ):
      result.class = ArgClass.Expand
    else:
      result.class = ArgClass.Indirect

  of tyInt .. tyInt64, tyUint .. tyUInt64:
    result.class = ArgClass.Direct

  of tyFloat32, tyFloat64, tyFloat:
    result.class = ArgClass.Direct

  of tyPtr, tyPointer, tyVar, tyCString, tyRef, tyString:
    result.class = ArgClass.Direct

  of tyOpenArray, tyVarargs:
    result.class = OpenArray

  of tyBool:
    result.class = Direct
    result.flags.incl Zext

  of tyProc:
    result.class = Direct

  of tyRange:
    result.class = Direct

  of tySet:
    let size = get_type_size(module, typ)
    case size:
    of 1, 2, 4, 8:
      result.class = Direct
    else:
      result.class = Indirect

  else:
    module.ice("classify_argument_type: " & $typ.kind)

method classify_return_type(abi: X86Abi; module: BModule; typ: PType): ArgInfo =
  case typ.kind:

  of tyArray:
    result.class = ArgClass.Indirect

  of tyObject, tyTuple:
    result.class = ArgClass.Indirect

  of tyInt .. tyInt64, tyUint .. tyUInt64, tyFloat32, tyFloat64, tyFloat:
    result.class = ArgClass.Direct

  of tyCString, tyPtr, tyPointer, tyVar, tyRef, tyString:
    result.class = Direct

  of tyBool:
    result.class = Direct
    result.flags.incl Zext

  of tyProc:
    result.class = Direct

  of tyRange:
    result.class = Direct

  else:
    module.ice("classify_return_type: " & $typ.kind)

method get_func_info(abi: X86Abi): FuncInfo =
  discard

# ------------------------------------------------------------------------------

proc get_abi(module: BModule): BaseAbi =
  case module.abi:
  of PlatformABI.AMD64_SystemV:   result = Amd64AbiSystemV()
  of PlatformABI.AMD64_Windows:   result = Amd64AbiWindows()
  of PlatformABI.X86:             result = X86Abi()
  of PlatformABI.Generic:         result = GenericAbi()

# ------------------------------------------------------------------------------

proc map_call_conv(module: BModule; cc: TCallingConvention): llvm.CallConv =
  let os = module.module_list.config.target.targetOS
  let cpu = module.module_list.config.target.targetCPU

  case cpu:

  of cpuI386:
    case os:
    of osWindows:
      case cc:
      of ccDefault:       result = X86StdcallCallConv
      of ccStdCall:       result = X86StdcallCallConv
      of ccCDecl:         result = CCallConv
      of ccSafeCall:      result = X86StdcallCallConv
      of ccSysCall:       result = X86StdcallCallConv
      of ccInline:        result = X86StdcallCallConv
      of ccNoInline:      result = X86StdcallCallConv
      of ccFastCall:      result = X86FastcallCallConv
      of ccClosure:       result = X86StdcallCallConv
      of ccNoConvention:  result = X86StdcallCallConv
    of osLinux:
      case cc:
      of ccDefault:       result = CCallConv
      of ccStdCall:       result = X86StdcallCallConv
      of ccCDecl:         result = CCallConv
      of ccSafeCall:      result = CCallConv
      of ccSysCall:       result = CCallConv
      of ccInline:        result = CCallConv
      of ccNoInline:      result = CCallConv
      of ccFastCall:      result = X86FastcallCallConv
      of ccClosure:       result = CCallConv
      of ccNoConvention:  result = CCallConv
    else: assert false, $cc
  # i386

  of cpuAmd64:
    case os:
    of osWindows:
      case cc:
      of ccDefault:       result = Win64CallConv
      of ccStdCall:       result = Win64CallConv
      of ccCDecl:         result = Win64CallConv
      of ccSafeCall:      result = Win64CallConv
      of ccSysCall:       result = Win64CallConv
      of ccInline:        result = Win64CallConv
      of ccNoInline:      result = Win64CallConv
      of ccFastCall:      result = Win64CallConv
      of ccClosure:       result = Win64CallConv
      of ccNoConvention:  result = Win64CallConv
    of osLinux:
      case cc:
      of ccDefault:       result = X8664SysVCallConv
      of ccStdCall:       result = X8664SysVCallConv
      of ccCDecl:         result = X8664SysVCallConv
      of ccSafeCall:      result = X8664SysVCallConv
      of ccSysCall:       result = X8664SysVCallConv
      of ccInline:        result = X8664SysVCallConv
      of ccNoInline:      result = X8664SysVCallConv
      of ccFastCall:      result = X8664SysVCallConv
      of ccClosure:       result = X8664SysVCallConv
      of ccNoConvention:  result = X8664SysVCallConv
    of osStandalone:
      case cc:
      of ccDefault:       result = X8664SysVCallConv
      of ccStdCall:       result = X8664SysVCallConv
      of ccCDecl:         result = X8664SysVCallConv
      of ccSafeCall:      result = X8664SysVCallConv
      of ccSysCall:       result = X8664SysVCallConv
      of ccInline:        result = X8664SysVCallConv
      of ccNoInline:      result = X8664SysVCallConv
      of ccFastCall:      result = X8664SysVCallConv
      of ccClosure:       result = X8664SysVCallConv
      of ccNoConvention:  result = X8664SysVCallConv
    else: assert false, $cc
  # amd64

  else: assert false, $cc
