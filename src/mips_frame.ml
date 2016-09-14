type access =
    InFrame of int
  | InReg of Temp.temp

type frame =
  {name: Temp.label;
   mutable formals: access list;
   mutable allocated: int}

type frag =
    STRING of Temp.label * string
  | PROC of Tree.stm * frame

let fp = Tree.TEMP (Temp.new_temp ())
let word_size = 4

let rv = Temp.new_temp ()

let alloc_local ({allocated; _} as frame) escape =
  if escape then
    let a = InFrame (-allocated * word_size) in
      frame.formals <- frame.formals @ [a];
      frame.allocated <- frame.allocated + 1;
      a
  else
    raise (Failure "alloc_local")

let new_frame name escapes =
  let f = {name; formals = []; allocated = 0} in
    List.iter (fun e -> ignore (alloc_local f e)) escapes;
    f

let name {name; _} = name
let formals {formals; _} = formals

let exp access fp =
  match access with
    InFrame k ->
      Tree.MEM (Tree.BINOP (Tree.PLUS, fp, Tree.CONST k))
  | InReg t ->
      Tree.TEMP t

let external_call name args =
  Tree.CALL (Tree.NAME (Temp.named_label name), args)

(* TODO: proc_entry_exit1 *)
let proc_entry_exit1 frame body = body
