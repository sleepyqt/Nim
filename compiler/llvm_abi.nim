import ast, types
import llvm_data
import llvm_dll as llvm

# ------------------------------------------------------------------------------

type ArgClass* = enum
  # Passed directly in register
  Direct

  # Passed indirectly via a pointer
  Indirect

  # object or tuple expandend into consecutive arguments
  Expand

  Ignore

  OpenArray

type ArgFlag* = enum
  ByVal, InReg, Sext, Zext, ExpandToWords

type ArgInfo* = object
  class*: ArgClass
  flags*: set[ArgFlag]

# ------------------------------------------------------------------------------

type BaseAbi* = ref object of RootObj

method classify_argument_type*(abi: BaseAbi; module: BModule; typ: PType): ArgInfo {.base.} =
  discard

method classify_return_type*(abi: BaseAbi; module: BModule; typ: PType): ArgInfo {.base.} =
  discard

# ------------------------------------------------------------------------------

type GenericAbi* = ref object of BaseAbi

method classify_argument_type*(abi: GenericAbi; module: BModule; typ: PType): ArgInfo =
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

method classify_return_type*(abi: GenericAbi; module: BModule; typ: PType): ArgInfo =
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

# ------------------------------------------------------------------------------

type Amd64AbiSystemV* = ref object of BaseAbi
  sse_regs*: int
  int_regs*: int

method classify_argument_type*(abi: Amd64AbiSystemV; module: BModule; typ: PType): ArgInfo =
  case typ.kind:

  of tyArray:
    result.class = Indirect

  of tyObject, tyTuple:
    result.class = Expand
    result.flags.incl ExpandToWords

  of tyInt .. tyInt64, tyUint .. tyUInt64:
    result.class = Direct

  of tyFloat32, tyFloat64, tyFloat:
    result.class = Direct

  of tyChar:
    result.class = Direct

  of tyCString, tyPtr, tyPointer, tyVar, tyRef, tyString:
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

  else:
    module.ice("classify_argument_type: " & $typ.kind)

method classify_return_type*(abi: Amd64AbiSystemV; module: BModule; typ: PType): ArgInfo =
  case typ.kind:

  of tyArray:
    result.class = Indirect

  of tyObject, tyTuple:
    result.class = Indirect

  of tyInt .. tyInt64, tyUint .. tyUInt64:
    result.class = Direct

  of tyFloat32, tyFloat64, tyFloat:
    result.class = Direct

  of tyChar:
    result.class = Direct

  of tyCString, tyPtr, tyPointer, tyVar, tyRef, tyString:
    result.class = Direct

  of tyBool:
    result.class = Direct
    result.flags.incl Zext

  of tyOpenArray, tyVarargs:
    result.class = OpenArray

  of tyProc:
    result.class = Direct

  else:
    module.ice("classify_return_type: " & $typ.kind)

# ------------------------------------------------------------------------------

type Amd64AbiWindows* = ref object of BaseAbi

method classify_argument_type*(abi: Amd64AbiWindows; module: BModule; typ: PType): ArgInfo =
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

method classify_return_type*(abi: Amd64AbiWindows; module: BModule; typ: PType): ArgInfo =
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

# ------------------------------------------------------------------------------

type X86Abi* = ref object of BaseAbi

proc x86_can_expand(struct: PType): bool =
  result = true

method classify_argument_type*(abi: X86Abi; module: BModule; typ: PType): ArgInfo =
  case typ.kind:

  of tyArray:
    result.class = ArgClass.Indirect

  of tyObject, tyTuple:
    if x86_can_expand(typ):
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

  else:
    module.ice("classify_argument_type: " & $typ.kind)

method classify_return_type*(abi: X86Abi; module: BModule; typ: PType): ArgInfo =
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

  else:
    module.ice("classify_return_type: " & $typ.kind)

# ------------------------------------------------------------------------------

proc get_abi*(module: BModule): BaseAbi =
  case module.abi:
  of PlatformABI.AMD64_SystemV:   result = Amd64AbiSystemV()
  of PlatformABI.AMD64_Windows:   result = Amd64AbiWindows()
  of PlatformABI.X86:             result = X86Abi()
  of PlatformABI.Generic:         result = GenericAbi()
