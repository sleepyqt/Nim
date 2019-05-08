import llvm_data, llvm_type
import llvm_dll as llvm
import ast

type PathNode = object
  index: int
  node: PNode

proc find_field(module: BModule; field: PSym; node: PNode; path: var seq[PathNode]): bool =
  case node.kind:
  of nkRecList:
    path.add PathNode(index: 0, node: node)
    for item in node:
      if find_field(module, field, item, path):
        return true
      else:
        inc path[^1].index
    path.del(path.high)
  of nkRecCase:
    if node[0].sym.id == field.id:
      return true
    inc path[^1].index
    for item in node.sons[1 .. ^1]:
      if item.kind in {nkOfBranch, nkElse}:
        if find_field(module, field, item.lastSon, path):
          return true
  of nkSym:
    if node.sym.id == field.id:
      return true
  else:
    discard

proc gen_field_access*(module: BModule; object_type: PType; object_value: ValueRef; field: PSym): ValueRef =
  # returns pointer to object field

  var path: seq[PathNode]
  var found = false
  var current = object_type
  while true:
    set_len(path, 0)
    let super = if current.sons.len == 0: nil else: current[0]
    let found = find_field(module, field, current.n, path)
    if found or (current == nil): break
    current = super

  assert path.len > 0

  if current.sons.len != 0 and current[0] != nil:
    inc path[0].index # skip superclass field

  if current.sons.len == 0:
    if (tfFinal notin current.flags) and (current.sym != nil and sfPure notin current.sym.flags):
      inc path[0].index # skip m_type

  let object_adr =
    if current != object_type:
      llvm.buildBitCast(module.ll_builder, object_value, type_to_ptr get_type(module, current), "super_cast")
    else:
      object_value

  var indices = [ constant(module, 0i32), constant(module, path[0].index.int32) ]
  result = llvm.buildGEP(module.ll_builder, object_adr, addr indices[0], cuint len indices, "")

  for i in 1 .. path.high:
    let brach_type = llvm.pointerType(get_object_case_branch_type(module, path[i].node), 0)

    var indices = [ constant(module, 0i32), constant(module, path[i].index.int32) ]
    let bitcast = llvm.buildBitCast(module.ll_builder, result, brach_type, "branch_cast")
    result = llvm.buildGEP(module.ll_builder, bitcast, addr indices[0], cuint len indices, "")
