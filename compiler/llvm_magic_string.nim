proc gen_magic_length_str(module: BModule; node: PNode): ValueRef =
  case node[1].typ.kind:
  of tyCString:
    var args: seq[ValueRef]
    args.add gen_expr(module, node[1])
    result = gen_call_runtime_proc(module, "nimCStrLen", args)
  of tyString:
    assert false
  else:
    assert false

proc gen_magic_new_string(module: BModule; node: PNode): ValueRef =
  #debug node
  #result = gen_call_expr(module, node)
  result = gen_call_runtime_proc(module, node)
