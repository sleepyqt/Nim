import std / dynlib

when defined(linux):
  const llvm_dlls = [ "libLLVM-7.0.0.so", "libLLVM-8.0.0.so" ]
  {.pragma: dll, cdecl.}

when defined(windows):
  const llvm_dlls = [ "LLVM-7.0.0.dll", "LLVM-8.0.0.dll" ]
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
  AttributeRef* = ptr object
  UseRef* = ptr object
  TargetDataRef* = ptr object

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

  IntPredicate* = enum
    IntEQ = 32,                 ## *< equal
    IntNE,                    ## *< not equal
    IntUGT,                   ## *< unsigned greater than
    IntUGE,                   ## *< unsigned greater or equal
    IntULT,                   ## *< unsigned less than
    IntULE,                   ## *< unsigned less or equal
    IntSGT,                   ## *< signed greater than
    IntSGE,                   ## *< signed greater or equal
    IntSLT,                   ## *< signed less than
    IntSLE                    ## *< signed less or equal

  RealPredicate* = enum
    RealPredicateFalse,       ## *< Always false (always folded)
    RealOEQ,                  ## *< True if ordered and equal
    RealOGT,                  ## *< True if ordered and greater than
    RealOGE,                  ## *< True if ordered and greater than or equal
    RealOLT,                  ## *< True if ordered and less than
    RealOLE,                  ## *< True if ordered and less than or equal
    RealONE,                  ## *< True if ordered and operands are unequal
    RealORD,                  ## *< True if ordered (no nans)
    RealUNO,                  ## *< True if unordered: isnan(X) | isnan(Y)
    RealUEQ,                  ## *< True if unordered or equal
    RealUGT,                  ## *< True if unordered or greater than
    RealUGE,                  ## *< True if unordered, greater than, or equal
    RealULT,                  ## *< True if unordered or less than
    RealULE,                  ## *< True if unordered, less than, or equal
    RealUNE,                  ## *< True if unordered or not equal
    RealPredicateTrue         ## *< Always true (always folded)

  VerifierFailureAction* = enum
    AbortProcessAction,       ##  verifier will print to stderr and abort()
    PrintMessageAction,       ##  verifier will print to stderr and return 1
    ReturnStatusAction        ##  verifier will just return 1

  Opcode* = enum
    #  Terminator Instructions
    Ret = 1, Br = 2, Switch = 3, IndirectBr = 4, Invoke = 5,
    #  removed 6 due to API changes
    Unreachable = 7,
    #  Standard Binary Operators
    Add = 8, FAdd = 9, Sub = 10, FSub = 11, Mul = 12, FMul = 13, UDiv = 14, SDiv = 15, FDiv = 16,
    URem = 17, SRem = 18, FRem = 19,
    #  Logical Operators
    Shl = 20, LShr = 21, AShr = 22, And = 23, Or = 24, Xor = 25,
    #  Memory Operators
    Alloca = 26, Load = 27, Store = 28, GetElementPtr = 29,
    #  Cast Operators
    Trunc = 30, ZExt = 31, SExt = 32, FPToUI = 33, FPToSI = 34, UIToFP = 35, SIToFP = 36,
    FPTrunc = 37, FPExt = 38, PtrToInt = 39, IntToPtr = 40, BitCast = 41, ICmp = 42, FCmp = 43,
    PHI = 44, Call = 45, Select = 46, UserOp1 = 47, UserOp2 = 48, VAArg = 49, ExtractElement = 50,
    InsertElement = 51, ShuffleVector = 52, ExtractValue = 53, InsertValue = 54,
    #  Atomic operators
    Fence = 55, AtomicCmpXchg = 56, AtomicRMW = 57,
    #  Exception Handling Operators
    Resume = 58, LandingPad = 59, AddrSpaceCast = 60,
    #  Other Operators
    CleanupRet = 61, CatchRet = 62, CatchPad = 63, CleanupPad = 64, CatchSwitch = 65

  AttributeIndex* = cuint

  TypeKind* = enum
    VoidTypeKind,             ## *< type with no size
    HalfTypeKind,             ## *< 16 bit floating point type
    FloatTypeKind,            ## *< 32 bit floating point type
    DoubleTypeKind,           ## *< 64 bit floating point type
    X86FP80TypeKind,          ## *< 80 bit floating point type (X87)
    FP128TypeKind,            ## *< 128 bit floating point type (112-bit mantissa)
    PPC_FP128TypeKind,        ## *< 128 bit floating point type (two 64-bits)
    LabelTypeKind,            ## *< Labels
    IntegerTypeKind,          ## *< Arbitrary bit width integers
    FunctionTypeKind,         ## *< Functions
    StructTypeKind,           ## *< Structures
    ArrayTypeKind,            ## *< Arrays
    PointerTypeKind,          ## *< Pointers
    VectorTypeKind,           ## *< SIMD 'packed' format, or other vector type
    MetadataTypeKind,         ## *< Metadata
    X86MMXTypeKind,           ## *< X86 MMX
    TokenTypeKind             ## *< Tokens

  ValueKind* = enum
    ArgumentValueKind, BasicBlockValueKind, MemoryUseValueKind, MemoryDefValueKind,
    MemoryPhiValueKind, FunctionValueKind, GlobalAliasValueKind,
    GlobalIFuncValueKind, GlobalVariableValueKind, BlockAddressValueKind,
    ConstantExprValueKind, ConstantArrayValueKind, ConstantStructValueKind,
    ConstantVectorValueKind, UndefValueValueKind, ConstantAggregateZeroValueKind,
    ConstantDataArrayValueKind, ConstantDataVectorValueKind, ConstantIntValueKind,
    ConstantFPValueKind, ConstantPointerNullValueKind, ConstantTokenNoneValueKind,
    MetadataAsValueValueKind, InlineAsmValueKind, InstructionValueKind

type
  BinaryProc* = proc (b: BuilderRef; l, r: ValueRef; n: cstring): ValueRef {.dll.}
  UnaryProc* = proc(b: BuilderRef; v: ValueRef; name: cstring): ValueRef {.dll.}

# ------------------------------------------------------------------------------

var contextCreate*: proc (): ContextRef {.dll.}
var moduleCreateWithNameInContext*: proc(moduleID: cstring; c: ContextRef): ModuleRef {.dll.}

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

var pointerType*: proc(elementType: TypeRef; addressSpace: cuint): TypeRef {.dll.}
var arrayType*: proc(elementType: TypeRef; elementCount: cuint): TypeRef {.dll.}
var getElementType*: proc(ty: TypeRef): TypeRef {.dll.}
var getArrayLength*: proc(arrayTy: TypeRef): cuint {.dll.}

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
var getEntryBasicBlock*: proc(fn: ValueRef): BasicBlockRef {.dll.}
var deleteBasicBlock*: proc(bb: BasicBlockRef) {.dll.}

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

var constInt*: proc(intTy: TypeRef; n: culonglong; signExtend: Bool): ValueRef {.dll.}

var buildICmp*: proc(a2: BuilderRef; op: IntPredicate; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildFCmp*: proc(a2: BuilderRef; op: RealPredicate; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}

var getBasicBlockParent*: proc(bb: BasicBlockRef): ValueRef {.dll.}
var getBasicBlockTerminator*: proc(bb: BasicBlockRef): ValueRef {.dll.}

var verifyModule*: proc(m: ModuleRef; action: VerifierFailureAction; outMessage: cstringArray): Bool {.dll.}
var verifyFunction*: proc(fn: ValueRef; action: VerifierFailureAction): Bool {.dll.}
var viewFunctionCFG*: proc(fn: ValueRef) {.dll.}
var viewFunctionCFGOnly*: proc(fn: ValueRef) {.dll.}

var buildResume*: proc(b: BuilderRef; exn: ValueRef): ValueRef {.dll.}
var buildLandingPad*: proc(b: BuilderRef; ty: TypeRef; persFn: ValueRef; numClauses: cuint; name: cstring): ValueRef {.dll.}
var buildCleanupRet*: proc(b: BuilderRef; catchPad: ValueRef; bb: BasicBlockRef): ValueRef {.dll.}
var buildCatchRet*: proc(b: BuilderRef; catchPad: ValueRef; bb: BasicBlockRef): ValueRef {.dll.}
var buildCatchPad*: proc(b: BuilderRef; parentPad: ValueRef; args: ptr ValueRef; numArgs: cuint; name: cstring): ValueRef {.dll.}
var buildCleanupPad*: proc(b: BuilderRef; parentPad: ValueRef; args: ptr ValueRef; numArgs: cuint; name: cstring): ValueRef {.dll.}
var buildCatchSwitch*: proc(b: BuilderRef; parentPad: ValueRef; unwindBB: BasicBlockRef; numHandlers: cuint; name: cstring): ValueRef {.dll.}
var addCase*: proc(switch: ValueRef; onVal: ValueRef; dest: BasicBlockRef) {.dll.}
var addDestination*: proc(indirectBr: ValueRef; dest: BasicBlockRef) {.dll.}
var getNumClauses*: proc(landingPad: ValueRef): cuint {.dll.}
var getClause*: proc(landingPad: ValueRef; idx: cuint): ValueRef {.dll.}
var addClause*: proc(landingPad: ValueRef; clauseVal: ValueRef) {.dll.}
var isCleanup*: proc(landingPad: ValueRef): Bool {.dll.}
var setCleanup*: proc(landingPad: ValueRef; val: Bool) {.dll.}
var addHandler*: proc(catchSwitch: ValueRef; dest: BasicBlockRef) {.dll.}
var getNumHandlers*: proc(catchSwitch: ValueRef): cuint {.dll.}

var buildAlloca*: proc(a2: BuilderRef; ty: TypeRef; name: cstring): ValueRef {.dll.}
var buildArrayAlloca*: proc(a2: BuilderRef; ty: TypeRef; val: ValueRef; name: cstring): ValueRef {.dll.}
var buildFree*: proc(a2: BuilderRef; pointerVal: ValueRef): ValueRef {.dll.}
var buildLoad*: proc(a2: BuilderRef; pointerVal: ValueRef; name: cstring): ValueRef {.dll.}
var buildStore*: proc(a2: BuilderRef; val: ValueRef; `ptr`: ValueRef): ValueRef {.dll.}
var buildGEP*: proc(b: BuilderRef; pointer: ValueRef; indices: ptr ValueRef; numIndices: cuint; name: cstring): ValueRef {.dll.}
var buildInBoundsGEP*: proc(b: BuilderRef; pointer: ValueRef; indices: ptr ValueRef; numIndices: cuint; name: cstring): ValueRef {.dll.}
var buildStructGEP*: proc(b: BuilderRef; pointer: ValueRef; idx: cuint; name: cstring): ValueRef {.dll.}
var buildGlobalString*: proc(b: BuilderRef; str: cstring; name: cstring): ValueRef {.dll.}
var buildGlobalStringPtr*: proc(b: BuilderRef; str: cstring; name: cstring): ValueRef {.dll.}
var buildAdd*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildNSWAdd*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildNUWAdd*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildFAdd*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildSub*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildNSWSub*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildNUWSub*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildFSub*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildMul*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildNSWMul*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildNUWMul*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildFMul*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildUDiv*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildExactUDiv*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildSDiv*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildExactSDiv*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildFDiv*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildURem*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildSRem*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildFRem*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildShl*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildLShr*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildAShr*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildAnd*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildOr*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildXor*: proc(a2: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildBinOp*: proc(b: BuilderRef; op: Opcode; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.dll.}
var buildNeg*: proc(a2: BuilderRef; v: ValueRef; name: cstring): ValueRef {.dll.}
var buildNSWNeg*: proc(b: BuilderRef; v: ValueRef; name: cstring): ValueRef {.dll.}
var buildNUWNeg*: proc(b: BuilderRef; v: ValueRef; name: cstring): ValueRef {.dll.}
var buildFNeg*: proc(a2: BuilderRef; v: ValueRef; name: cstring): ValueRef {.dll.}
var buildNot*: proc(a2: BuilderRef; v: ValueRef; name: cstring): ValueRef {.dll.}
var buildTrunc*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildZExt*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildSExt*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildFPToUI*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildFPToSI*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildUIToFP*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildSIToFP*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildFPTrunc*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildFPExt*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildPtrToInt*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildIntToPtr*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildBitCast*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildAddrSpaceCast*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildZExtOrBitCast*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildSExtOrBitCast*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildTruncOrBitCast*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildCast*: proc(b: BuilderRef; op: Opcode; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildPointerCast*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildIntCast*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}
var buildFPCast*: proc(a2: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.dll.}

var buildCall*: proc(a2: BuilderRef; fn: ValueRef; args: ptr ValueRef; numArgs: cuint; name: cstring): ValueRef {.dll.}
var getNamedFunction*: proc(m: ModuleRef; name: cstring): ValueRef {.dll.}

var getParam*: proc(fn: ValueRef; index: cuint): ValueRef {.dll.}
var setValueName2*: proc(val: ValueRef; name: cstring; nameLen: csize) {.dll.}

var addAttributeAtIndex*: proc(f: ValueRef; idx: AttributeIndex; a: AttributeRef) {.dll.}
var getTypeKind*: proc(ty: TypeRef): TypeKind {.dll.}

var getEnumAttributeKindForName*: proc(name: cstring; sLen: csize): cuint {.dll.}
var createEnumAttribute*: proc(c: ContextRef; kindID: cuint; val: uint64): AttributeRef {.dll.}

var addPromoteMemoryToRegisterPass*: proc(pm: PassManagerRef) {.dll.}
var addCFGSimplificationPass*: proc(pm: PassManagerRef) {.dll.}
var addReassociatePass*: proc(pm: PassManagerRef) {.dll.}
var addInstructionCombiningPass*: proc(pm: PassManagerRef) {.dll.}
var createPassManager*: proc(): PassManagerRef {.dll.}
var runPassManager*: proc(pm: PassManagerRef; m: ModuleRef): Bool {.dll.}
var disposePassManager*: proc(pm: PassManagerRef) {.dll.}
var addNewGVNPass*: proc(pm: PassManagerRef) {.dll.}

var instructionEraseFromParent*: proc(inst: ValueRef) {.dll.}
var getInstructionOpcode*: proc(inst: ValueRef): Opcode {.dll.}
var getValueKind*: proc(val: ValueRef): ValueKind {.dll.}
var getOperand*: proc(val: ValueRef; index: cuint): ValueRef {.dll.}
var getOperandUse*: proc(val: ValueRef; index: cuint): UseRef {.dll.}

var getFirstUse*: proc(val: ValueRef): UseRef {.dll.}
var getNextUse*: proc(u: UseRef): UseRef {.dll.}
var getUser*: proc(u: UseRef): ValueRef {.dll.}
var getUsedValue*: proc(u: UseRef): ValueRef {.dll.}

var addIncoming*: proc(phiNode: ValueRef; incomingValues: ptr ValueRef; incomingBlocks: ptr BasicBlockRef; count: cuint) {.dll.}
var countIncoming*: proc(phiNode: ValueRef): cuint {.dll.}
var getIncomingValue*: proc(phiNode: ValueRef; index: cuint): ValueRef {.dll.}
var getIncomingBlock*: proc(phiNode: ValueRef; index: cuint): BasicBlockRef {.dll.}
var buildPhi*: proc(a2: BuilderRef; ty: TypeRef; name: cstring): ValueRef {.dll.}

var constStringInContext*: proc(c: ContextRef; str: cstring; length: cuint; dontNullTerminate: Bool): ValueRef {.dll.}
var constStructInContext*: proc(c: ContextRef; constantVals: ptr ValueRef; count: cuint; packed: Bool): ValueRef {.dll.}
var constArray*: proc(elementTy: TypeRef; constantVals: ptr ValueRef; length: cuint): ValueRef {.dll.}

var addGlobal*: proc(m: ModuleRef; ty: TypeRef; name: cstring): ValueRef {.dll.}
var getNamedGlobal*: proc(m: ModuleRef; name: cstring): ValueRef {.dll.}
var setInitializer*: proc(globalVar: ValueRef; constantVal: ValueRef) {.dll.}

var constNull*: proc(ty: TypeRef): ValueRef {.dll.}
var constAllOnes*: proc(ty: TypeRef): ValueRef {.dll.}
var getUndef*: proc(ty: TypeRef): ValueRef {.dll.}

var getIntTypeWidth*: proc(integerTy: TypeRef): cuint {.dll.}

var constReal*: proc(realTy: TypeRef; n: cdouble): ValueRef {.dll.}
var buildSelect*: proc(a2: BuilderRef; `if`: ValueRef; then: ValueRef; `else`: ValueRef; name: cstring): ValueRef {.dll.}
var setGlobalConstant*: proc(globalVar: ValueRef; isConstant: Bool) {.dll.}

var buildExtractValue*: proc(a2: BuilderRef; aggVal: ValueRef; index: cuint; name: cstring): ValueRef {.dll.}
var buildInsertValue*: proc(a2: BuilderRef; aggVal: ValueRef; eltVal: ValueRef; index: cuint; name: cstring): ValueRef {.dll.}

var setSourceFileName*: proc(m: ModuleRef; name: cstring; len: csize) {.dll.}
var setDataLayout*: proc(m: ModuleRef; dataLayoutStr: cstring) {.dll.}
var setModuleDataLayout*: proc(m: ModuleRef; dl: TargetDataRef) {.dll.}
var setTarget*: proc(m: ModuleRef; triple: cstring) {.dll.}
var createTargetDataLayout*: proc(t: TargetMachineRef): TargetDataRef {.dll.}

var countStructElementTypes*: proc(structTy: TypeRef): cuint {.dll.}
var getStructElementTypes*: proc(structTy: TypeRef; dest: ptr TypeRef) {.dll.}
var structGetTypeAtIndex*: proc(structTy: TypeRef; i: cuint): TypeRef {.dll.}
var isPackedStruct*: proc(structTy: TypeRef): Bool {.dll.}
var isOpaqueStruct*: proc(structTy: TypeRef): Bool {.dll.}

# ------------------------------------------------------------------------------

template get_proc(lib: typed; fun: pointer; name: typed): typed =
  fun = cast[type(fun)](symAddr(lib, name))
  if fun == nil: echo "load fail: ", name

proc ll_load_dll*: bool =
  var lib: LibHandle

  for dll in llvm_dlls:
    lib = loadLib(dll)
    if lib != nil: break

  if lib == nil:
    echo "can't load LLVM dll :( :: ", llvm_dlls

  result = lib != nil
  if result:
    get_proc(lib, contextCreate, "LLVMContextCreate")
    get_proc(lib, moduleCreateWithNameInContext, "LLVMModuleCreateWithNameInContext")

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

    get_proc(lib, pointerType, "LLVMPointerType")
    get_proc(lib, arrayType, "LLVMArrayType")
    get_proc(lib, getElementType, "LLVMGetElementType")
    get_proc(lib, getArrayLength, "LLVMGetArrayLength")

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
    get_proc(lib, getEntryBasicBlock, "LLVMGetEntryBasicBlock")
    get_proc(lib, deleteBasicBlock, "LLVMDeleteBasicBlock")

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

    get_proc(lib, constInt, "LLVMConstInt")

    get_proc(lib, buildICmp, "LLVMBuildICmp")
    get_proc(lib, buildFCmp, "LLVMBuildFCmp")

    get_proc(lib, getBasicBlockParent, "LLVMGetBasicBlockParent")
    get_proc(lib, getBasicBlockTerminator, "LLVMGetBasicBlockTerminator")

    get_proc(lib, verifyModule, "LLVMVerifyModule")
    get_proc(lib, verifyFunction, "LLVMVerifyFunction")
    get_proc(lib, viewFunctionCFG, "LLVMViewFunctionCFG")
    get_proc(lib, viewFunctionCFGOnly, "LLVMViewFunctionCFGOnly")

    get_proc(lib, buildResume, "LLVMBuildResume")
    get_proc(lib, buildLandingPad, "LLVMBuildLandingPad")
    get_proc(lib, buildCleanupRet, "LLVMBuildCleanupRet")
    get_proc(lib, buildCatchRet, "LLVMBuildCatchRet")
    get_proc(lib, buildCatchPad, "LLVMBuildCatchPad")
    get_proc(lib, buildCleanupPad, "LLVMBuildCleanupPad")
    get_proc(lib, buildCatchSwitch, "LLVMBuildCatchSwitch")
    get_proc(lib, addCase, "LLVMAddCase")
    get_proc(lib, addDestination, "LLVMAddDestination")
    get_proc(lib, getNumClauses, "LLVMGetNumClauses")
    get_proc(lib, getClause, "LLVMGetClause")
    get_proc(lib, addClause, "LLVMAddClause")
    get_proc(lib, isCleanup, "LLVMIsCleanup")
    get_proc(lib, setCleanup, "LLVMSetCleanup")
    get_proc(lib, addHandler, "LLVMAddHandler")
    get_proc(lib, getNumHandlers, "LLVMGetNumHandlers")

    get_proc(lib, buildAlloca, "LLVMBuildAlloca")
    get_proc(lib, buildArrayAlloca, "LLVMBuildArrayAlloca")
    get_proc(lib, buildFree, "LLVMBuildFree")
    get_proc(lib, buildLoad, "LLVMBuildLoad")
    get_proc(lib, buildStore, "LLVMBuildStore")
    get_proc(lib, buildGEP, "LLVMBuildGEP")
    get_proc(lib, buildInBoundsGEP, "LLVMBuildInBoundsGEP")
    get_proc(lib, buildStructGEP, "LLVMBuildStructGEP")
    get_proc(lib, buildGlobalString, "LLVMBuildGlobalString")
    get_proc(lib, buildGlobalStringPtr, "LLVMBuildGlobalStringPtr")
    get_proc(lib, buildAdd, "LLVMBuildAdd")
    get_proc(lib, buildNSWAdd, "LLVMBuildNSWAdd")
    get_proc(lib, buildNUWAdd, "LLVMBuildNUWAdd")
    get_proc(lib, buildFAdd, "LLVMBuildFAdd")
    get_proc(lib, buildSub, "LLVMBuildSub")
    get_proc(lib, buildNSWSub, "LLVMBuildNSWSub")
    get_proc(lib, buildNUWSub, "LLVMBuildNUWSub")
    get_proc(lib, buildFSub, "LLVMBuildFSub")
    get_proc(lib, buildMul, "LLVMBuildMul")
    get_proc(lib, buildNSWMul, "LLVMBuildNSWMul")
    get_proc(lib, buildNUWMul, "LLVMBuildNUWMul")
    get_proc(lib, buildFMul, "LLVMBuildFMul")
    get_proc(lib, buildUDiv, "LLVMBuildUDiv")
    get_proc(lib, buildExactUDiv, "LLVMBuildExactUDiv")
    get_proc(lib, buildSDiv, "LLVMBuildSDiv")
    get_proc(lib, buildExactSDiv, "LLVMBuildExactSDiv")
    get_proc(lib, buildFDiv, "LLVMBuildFDiv")
    get_proc(lib, buildURem, "LLVMBuildURem")
    get_proc(lib, buildSRem, "LLVMBuildSRem")
    get_proc(lib, buildFRem, "LLVMBuildFRem")
    get_proc(lib, buildShl, "LLVMBuildShl")
    get_proc(lib, buildLShr, "LLVMBuildLShr")
    get_proc(lib, buildAShr, "LLVMBuildAShr")
    get_proc(lib, buildAnd, "LLVMBuildAnd")
    get_proc(lib, buildOr, "LLVMBuildOr")
    get_proc(lib, buildXor, "LLVMBuildXor")
    get_proc(lib, buildBinOp, "LLVMBuildBinOp")
    get_proc(lib, buildNeg, "LLVMBuildNeg")
    get_proc(lib, buildNSWNeg, "LLVMBuildNSWNeg")
    get_proc(lib, buildNUWNeg, "LLVMBuildNUWNeg")
    get_proc(lib, buildFNeg, "LLVMBuildFNeg")
    get_proc(lib, buildNot, "LLVMBuildNot")
    get_proc(lib, buildTrunc, "LLVMBuildTrunc")
    get_proc(lib, buildZExt, "LLVMBuildZExt")
    get_proc(lib, buildSExt, "LLVMBuildSExt")
    get_proc(lib, buildFPToUI, "LLVMBuildFPToUI")
    get_proc(lib, buildFPToSI, "LLVMBuildFPToSI")
    get_proc(lib, buildUIToFP, "LLVMBuildUIToFP")
    get_proc(lib, buildSIToFP, "LLVMBuildSIToFP")
    get_proc(lib, buildFPTrunc, "LLVMBuildFPTrunc")
    get_proc(lib, buildFPExt, "LLVMBuildFPExt")
    get_proc(lib, buildPtrToInt, "LLVMBuildPtrToInt")
    get_proc(lib, buildIntToPtr, "LLVMBuildIntToPtr")
    get_proc(lib, buildBitCast, "LLVMBuildBitCast")
    get_proc(lib, buildAddrSpaceCast, "LLVMBuildAddrSpaceCast")
    get_proc(lib, buildZExtOrBitCast, "LLVMBuildZExtOrBitCast")
    get_proc(lib, buildSExtOrBitCast, "LLVMBuildSExtOrBitCast")
    get_proc(lib, buildTruncOrBitCast, "LLVMBuildTruncOrBitCast")
    get_proc(lib, buildCast, "LLVMBuildCast")
    get_proc(lib, buildPointerCast, "LLVMBuildPointerCast")
    get_proc(lib, buildIntCast, "LLVMBuildIntCast")
    get_proc(lib, buildFPCast, "LLVMBuildFPCast")

    get_proc(lib, buildCall, "LLVMBuildCall")
    get_proc(lib, getNamedFunction, "LLVMGetNamedFunction")

    get_proc(lib, getParam, "LLVMGetParam")
    get_proc(lib, setValueName2, "LLVMSetValueName2")

    get_proc(lib, addAttributeAtIndex, "LLVMAddAttributeAtIndex")
    get_proc(lib, getTypeKind, "LLVMGetTypeKind")

    get_proc(lib, getEnumAttributeKindForName, "LLVMGetEnumAttributeKindForName")
    get_proc(lib, createEnumAttribute, "LLVMCreateEnumAttribute")

    get_proc(lib, addPromoteMemoryToRegisterPass, "LLVMAddPromoteMemoryToRegisterPass")
    get_proc(lib, addCFGSimplificationPass, "LLVMAddCFGSimplificationPass")
    get_proc(lib, addReassociatePass, "LLVMAddReassociatePass")
    get_proc(lib, addInstructionCombiningPass,"LLVMAddInstructionCombiningPass")
    get_proc(lib, createPassManager, "LLVMCreatePassManager")
    get_proc(lib, runPassManager, "LLVMRunPassManager")
    get_proc(lib, disposePassManager, "LLVMDisposePassManager")
    get_proc(lib, addNewGVNPass, "LLVMAddNewGVNPass")

    get_proc(lib, instructionEraseFromParent, "LLVMInstructionEraseFromParent")
    get_proc(lib, getInstructionOpcode, "LLVMGetInstructionOpcode")
    get_proc(lib, getValueKind, "LLVMGetValueKind")
    get_proc(lib, getOperand, "LLVMGetOperand")
    get_proc(lib, getOperandUse, "LLVMGetOperandUse")

    get_proc(lib, getFirstUse, "LLVMGetFirstUse")
    get_proc(lib, getNextUse, "LLVMGetNextUse")
    get_proc(lib, getUser, "LLVMGetUser")
    get_proc(lib, getUsedValue, "LLVMGetUsedValue")

    get_proc(lib, addIncoming, "LLVMAddIncoming")
    get_proc(lib, countIncoming, "LLVMCountIncoming")
    get_proc(lib, getIncomingValue, "LLVMGetIncomingValue")
    get_proc(lib, getIncomingBlock, "LLVMGetIncomingBlock")
    get_proc(lib, buildPhi, "LLVMBuildPhi")

    get_proc(lib, constStringInContext, "LLVMConstStringInContext")
    get_proc(lib, constStructInContext, "LLVMConstStructInContext")
    get_proc(lib, constArray, "LLVMConstArray")

    get_proc(lib, addGlobal, "LLVMAddGlobal")
    get_proc(lib, getNamedGlobal, "LLVMGetNamedGlobal")
    get_proc(lib, setInitializer, "LLVMSetInitializer")

    get_proc(lib, constNull, "LLVMConstNull")
    get_proc(lib, constAllOnes, "LLVMConstAllOnes")
    get_proc(lib, getUndef, "LLVMGetUndef")

    get_proc(lib, getIntTypeWidth, "LLVMGetIntTypeWidth")

    get_proc(lib, constReal, "LLVMConstReal")

    get_proc(lib, buildSelect, "LLVMBuildSelect")

    get_proc(lib, setGlobalConstant, "LLVMSetGlobalConstant")

    get_proc(lib, buildExtractValue, "LLVMBuildExtractValue")
    get_proc(lib, buildInsertValue, "LLVMBuildInsertValue")

    get_proc(lib, setSourceFileName, "LLVMSetSourceFileName")
    get_proc(lib, setDataLayout, "LLVMSetDataLayout")

    get_proc(lib, setModuleDataLayout, "LLVMSetModuleDataLayout")
    get_proc(lib, setTarget, "LLVMSetTarget")
    get_proc(lib, createTargetDataLayout, "LLVMCreateTargetDataLayout")

    get_proc(lib, countStructElementTypes, "LLVMCountStructElementTypes")
    get_proc(lib, getStructElementTypes, "LLVMGetStructElementTypes")
    get_proc(lib, structGetTypeAtIndex, "LLVMStructGetTypeAtIndex")
    get_proc(lib, isPackedStruct, "LLVMIsPackedStruct")
    get_proc(lib, isOpaqueStruct, "LLVMIsOpaqueStruct")

# ------------------------------------------------------------------------------

proc set_value_name*(value: ValueRef; name: cstring) =
  if setValueName2 == nil:
    discard
  else:
    setValueName2(value, name, csize len name)
