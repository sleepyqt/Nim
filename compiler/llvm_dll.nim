import std / dynlib

when defined(linux):
  const llvm_dll = "libLLVM-7.0.0.so"
  {.pragma: dll, cdecl.}

when defined(windows):
  const llvm_dll = "LLVM-7.0.dll"
  {.pragma: dll, stdcall.}

# ------------------------------------------------------------------------------

type
  Bool* = cint
  ContextRef* = ptr object
  ModuleRef* = ptr object
  TypeRef* = ptr object
  ValueRef* = ptr object
  BasicBlockRef* = ptr object
  BuilderRef* = ptr object
  PassManagerRef* = ptr object
  PassRegistryRef* = ptr object

type
  CodeGenOptLevel* {.size: sizeof(cint).} = enum
    CodeGenLevelNone, CodeGenLevelLess, CodeGenLevelDefault, CodeGenLevelAggressive

  RelocMode* {.size: sizeof(cint).} = enum
    RelocDefault, RelocStatic, RelocPIC, RelocDynamicNoPic

  CodeModel* {.size: sizeof(cint).} = enum
    CodeModelDefault, CodeModelJITDefault, CodeModelSmall, CodeModelKernel,
    CodeModelMedium, CodeModelLarge

  CodeGenFileType* {.size: sizeof(cint).} = enum
    AssemblyFile, ObjectFile

  TargetMachineRef* = ptr object
  TargetRef* = ptr object

# ------------------------------------------------------------------------------

var contextCreate*: proc (): ContextRef {.dll.}
var moduleCreateWithName*: proc (moduleID: cstring): ModuleRef {.dll.}

var int1TypeInContext*: proc(c: ContextRef): TypeRef {.dll.}
var int8TypeInContext*: proc(c: ContextRef): TypeRef {.dll.}
var int16TypeInContext*: proc(c: ContextRef): TypeRef {.dll.}
var int32TypeInContext*: proc(c: ContextRef): TypeRef {.dll.}
var int64TypeInContext*: proc(c: ContextRef): TypeRef {.dll.}
var int128TypeInContext*: proc(c: ContextRef): TypeRef {.dll.}
var intTypeInContext*: proc(c: ContextRef; numBits: cuint): TypeRef {.dll.}
var floatTypeInContext*: proc(c: ContextRef): TypeRef {.dll.}
var doubleTypeInContext*: proc(c: ContextRef): TypeRef {.dll.}
var voidTypeInContext*: proc(c: ContextRef): TypeRef {.dll.}

var getTargetFromTriple*: proc(triple: cstring; t: ptr TargetRef; errorMessage: ptr cstring): Bool {.dll.}
var disposeMessage*: proc(message: cstring) {.dll.}
var createTargetMachine*: proc(t: TargetRef; triple: cstring; cpu: cstring; features: cstring; level: CodeGenOptLevel; reloc: RelocMode; codeModel: CodeModel): TargetMachineRef {.dll.}
var initializeCore*: proc(r: PassRegistryRef) {.dll.}
var shutdown*: proc() {.dll.}
var initializeX86Target*: proc() {.dll.}
var initializeX86TargetInfo*: proc() {.dll.}
var initializeX86TargetTargetMC*: proc() {.dll.}
var initializeX86AsmPrinter*: proc() {.dll.}
var initializeX86AsmParser*: proc() {.dll.}
var initializeX86Disassembler*: proc() {.dll.}
var getGlobalPassRegistry*: proc(): PassRegistryRef {.dll.}
var initializeTransformUtils*: proc(r: PassRegistryRef) {.dll.}
var initializeScalarOpts*: proc(r: PassRegistryRef) {.dll.}
var initializeObjCARCOpts*: proc(r: PassRegistryRef) {.dll.}
var initializeVectorization*: proc(r: PassRegistryRef) {.dll.}
var initializeInstCombine*: proc(r: PassRegistryRef) {.dll.}
var initializeIPO*: proc(r: PassRegistryRef) {.dll.}
var initializeInstrumentation*: proc(r: PassRegistryRef) {.dll.}
var initializeAnalysis*: proc(r: PassRegistryRef) {.dll.}
var initializeIPA*: proc(r: PassRegistryRef) {.dll.}
var initializeCodeGen*: proc(r: PassRegistryRef) {.dll.}
var initializeTarget*: proc(r: PassRegistryRef) {.dll.}
var printModuleToFile*: proc(m: ModuleRef; filename: cstring; errorMessage: ptr cstring): Bool {.dll.}
var printModuleToString*: proc(m: ModuleRef): cstring {.dll.}

var writeBitcodeToFile*: proc(m: ModuleRef; path: cstring): cint {.dll.}
var targetMachineEmitToFile*: proc(t: TargetMachineRef; m: ModuleRef; filename: cstring; codegen: CodeGenFileType; errorMessage: ptr cstring): Bool {.dll.}

var addFunction*: proc(m: ModuleRef; name: cstring; functionTy: TypeRef): ValueRef {.dll.}
var functionType*: proc(returnType: TypeRef; paramTypes: ptr TypeRef; paramCount: cuint; isVarArg: Bool): TypeRef {.dll.}

var printTypeToString*: proc(val: TypeRef): cstring {.dll.}
var printValueToString*: proc(val: ValueRef): cstring {.dll.}
var typeOf*: proc(val: ValueRef): TypeRef {.dll.}

var structTypeInContext*: proc(c: ContextRef; elementTypes: ptr TypeRef; elementCount: cuint; packed: Bool): TypeRef {.dll.}
var structCreateNamed*: proc(c: ContextRef; name: cstring): TypeRef {.dll.}
var structSetBody*: proc(structTy: TypeRef; elementTypes: ptr TypeRef; elementCount: cuint; packed: Bool) {.dll.}

var appendBasicBlockInContext*: proc(c: ContextRef; fn: ValueRef; name: cstring): BasicBlockRef {.dll.}
var insertBasicBlockInContext*: proc(c: ContextRef; bb: BasicBlockRef; name: cstring): BasicBlockRef {.dll.}
var moveBasicBlockBefore*: proc(bb: BasicBlockRef; movePos: BasicBlockRef) {.dll.}
var moveBasicBlockAfter*: proc(bb: BasicBlockRef; movePos: BasicBlockRef) {.dll.}
var getFirstInstruction*: proc(bb: BasicBlockRef): ValueRef {.dll.}
var getLastInstruction*: proc(bb: BasicBlockRef): ValueRef {.dll.}

var createBuilderInContext*: proc(c: ContextRef): BuilderRef {.dll.}
var positionBuilder*: proc(builder: BuilderRef; `block`: BasicBlockRef; instr: ValueRef) {.dll.}
var positionBuilderBefore*: proc(builder: BuilderRef; instr: ValueRef) {.dll.}
var positionBuilderAtEnd*: proc(builder: BuilderRef; `block`: BasicBlockRef) {.dll.}
var getInsertBlock*: proc(builder: BuilderRef): BasicBlockRef {.dll.}
var clearInsertionPosition*: proc(builder: BuilderRef) {.dll.}
var insertIntoBuilder*: proc(builder: BuilderRef; instr: ValueRef) {.dll.}
var insertIntoBuilderWithName*: proc(builder: BuilderRef; instr: ValueRef; name: cstring) {.dll.}
var disposeBuilder*: proc(builder: BuilderRef) {.dll.}

var buildRetVoid*: proc(a2: BuilderRef): ValueRef {.dll.}
var buildRet*: proc(a2: BuilderRef; v: ValueRef): ValueRef {.dll.}
var buildAggregateRet*: proc(a2: BuilderRef; retVals: ptr ValueRef; n: cuint): ValueRef {.dll.}
var buildBr*: proc(a2: BuilderRef; dest: BasicBlockRef): ValueRef {.dll.}
var buildCondBr*: proc(a2: BuilderRef; `if`: ValueRef; then: BasicBlockRef; `else`: BasicBlockRef): ValueRef {.dll.}
var buildSwitch*: proc(a2: BuilderRef; v: ValueRef; `else`: BasicBlockRef; numCases: cuint): ValueRef {.dll.}
var buildIndirectBr*: proc(b: BuilderRef; `addr`: ValueRef; numDests: cuint): ValueRef {.dll.}
var buildInvoke*: proc(a2: BuilderRef; fn: ValueRef; args: ptr ValueRef; numArgs: cuint; then: BasicBlockRef; catch: BasicBlockRef; name: cstring): ValueRef {.dll.}
var buildUnreachable*: proc(a2: BuilderRef): ValueRef {.dll.}

# -----------------------------------------------------------------------------

template get_proc(lib: typed; fun: pointer; name: typed): typed =
  fun = cast[type(fun)](symAddr(lib, name))
  if fun == nil: echo "load fail: ", name

proc ll_load_dll*: bool =
  let lib = loadLib(llvm_dll)
  result = lib != nil
  if result:
    get_proc(lib, contextCreate, "LLVMContextCreate")
    get_proc(lib, moduleCreateWithName, "LLVMModuleCreateWithName")

    get_proc(lib, int1TypeInContext, "LLVMInt1TypeInContext")
    get_proc(lib, int8TypeInContext, "LLVMInt8TypeInContext")
    get_proc(lib, int16TypeInContext, "LLVMInt16TypeInContext")
    get_proc(lib, int32TypeInContext, "LLVMInt32TypeInContext")
    get_proc(lib, int64TypeInContext, "LLVMInt64TypeInContext")
    get_proc(lib, int128TypeInContext, "LLVMInt128TypeInContext")
    get_proc(lib, intTypeInContext, "LLVMIntTypeInContext")
    get_proc(lib, floatTypeInContext, "LLVMFloatTypeInContext")
    get_proc(lib, doubleTypeInContext, "LLVMDoubleTypeInContext")
    get_proc(lib, voidTypeInContext, "LLVMVoidTypeInContext")

    get_proc(lib, getTargetFromTriple, "LLVMGetTargetFromTriple")
    get_proc(lib, disposeMessage, "LLVMDisposeMessage")
    get_proc(lib, createTargetMachine, "LLVMCreateTargetMachine")
    get_proc(lib, initializeCore, "LLVMInitializeCore")
    get_proc(lib, shutdown, "LLVMShutdown")
    get_proc(lib, initializeX86Target, "LLVMInitializeX86Target")
    get_proc(lib, initializeX86TargetInfo, "LLVMInitializeX86TargetInfo")
    get_proc(lib, initializeX86TargetTargetMC, "LLVMInitializeX86TargetMC")
    get_proc(lib, initializeX86AsmPrinter, "LLVMInitializeX86AsmPrinter")
    get_proc(lib, initializeX86AsmParser, "LLVMInitializeX86AsmParser")
    get_proc(lib, initializeX86Disassembler, "LLVMInitializeX86Disassembler")
    get_proc(lib, getGlobalPassRegistry, "LLVMGetGlobalPassRegistry")
    get_proc(lib, initializeTransformUtils, "LLVMInitializeTransformUtils")
    get_proc(lib, initializeScalarOpts, "LLVMInitializeScalarOpts")
    get_proc(lib, initializeObjCARCOpts, "LLVMInitializeObjCARCOpts")
    get_proc(lib, initializeVectorization, "LLVMInitializeVectorization")
    get_proc(lib, initializeInstCombine, "LLVMInitializeInstCombine")
    get_proc(lib, initializeIPO, "LLVMInitializeIPO")
    get_proc(lib, initializeInstrumentation, "LLVMInitializeInstrumentation")
    get_proc(lib, initializeAnalysis, "LLVMInitializeAnalysis")
    get_proc(lib, initializeIPA, "LLVMInitializeIPA")
    get_proc(lib, initializeCodeGen, "LLVMInitializeCodeGen")
    get_proc(lib, initializeTarget, "LLVMInitializeTarget")
    get_proc(lib, printModuleToFile, "LLVMPrintModuleToFile")
    get_proc(lib, printModuleToString, "LLVMPrintModuleToString")
    get_proc(lib, writeBitcodeToFile, "LLVMWriteBitcodeToFile")
    get_proc(lib, targetMachineEmitToFile, "LLVMTargetMachineEmitToFile")

    get_proc(lib, addFunction, "LLVMAddFunction")
    get_proc(lib, functionType, "LLVMFunctionType")

    get_proc(lib, printTypeToString, "LLVMPrintTypeToString")
    get_proc(lib, printValueToString, "LLVMPrintValueToString")
    get_proc(lib, typeOf, "LLVMTypeOf")

    get_proc(lib, structTypeInContext, "LLVMStructTypeInContext")
    get_proc(lib, structCreateNamed, "LLVMStructCreateNamed")
    get_proc(lib, structSetBody, "LLVMStructSetBody")

    get_proc(lib, appendBasicBlockInContext, "LLVMAppendBasicBlockInContext")
    get_proc(lib, insertBasicBlockInContext, "LLVMInsertBasicBlockInContext")
    get_proc(lib, moveBasicBlockBefore, "LLVMMoveBasicBlockBefore")
    get_proc(lib, moveBasicBlockAfter, "LLVMMoveBasicBlockAfter")
    get_proc(lib, getFirstInstruction, "LLVMGetFirstInstruction")
    get_proc(lib, getLastInstruction, "LLVMGetLastInstruction")

    get_proc(lib, createBuilderInContext, "LLVMCreateBuilderInContext")
    get_proc(lib, positionBuilder, "LLVMPositionBuilder")
    get_proc(lib, positionBuilderBefore, "LLVMPositionBuilderBefore")
    get_proc(lib, positionBuilderAtEnd, "LLVMPositionBuilderAtEnd")
    get_proc(lib, getInsertBlock, "LLVMGetInsertBlock")
    get_proc(lib, clearInsertionPosition, "LLVMClearInsertionPosition")
    get_proc(lib, insertIntoBuilder, "LLVMInsertIntoBuilder")
    get_proc(lib, insertIntoBuilderWithName, "LLVMInsertIntoBuilderWithName")
    get_proc(lib, disposeBuilder, "LLVMDisposeBuilder")

    get_proc(lib, buildRetVoid, "LLVMBuildRetVoid")
    get_proc(lib, buildRet, "LLVMBuildRet")
    get_proc(lib, buildAggregateRet, "LLVMBuildAggregateRet")
    get_proc(lib, buildBr, "LLVMBuildBr")
    get_proc(lib, buildCondBr, "LLVMBuildCondBr")
    get_proc(lib, buildSwitch, "LLVMBuildSwitch")
    get_proc(lib, buildIndirectBr, "LLVMBuildIndirectBr")
    get_proc(lib, buildInvoke, "LLVMBuildInvoke")
    get_proc(lib, buildUnreachable, "LLVMBuildUnreachable")
