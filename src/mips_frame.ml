type access =
    InFrame of int
  | InReg of Temp.temp

type frame =
  {name: Temp.label;
   formals: access list;
   mutable allocated: int;
   viewshift: Tree.stm}

type frag =
    STRING of Temp.label * string
  | PROC of Tree.stm * frame

type register = string

let word_size = 4

let fp = Temp.new_temp ()
let sp = Temp.new_temp ()
let ra = Temp.new_temp ()
let zero = Temp.new_temp ()
let gp = Temp.new_temp ()
let at = Temp.new_temp ()
let v0 = Temp.new_temp ()
let v1 = Temp.new_temp ()
let k0 = Temp.new_temp ()
let k1 = Temp.new_temp ()
let a0 = Temp.new_temp ()
let a1 = Temp.new_temp ()
let a2 = Temp.new_temp ()
let a3 = Temp.new_temp ()
let s0 = Temp.new_temp ()
let s1 = Temp.new_temp ()
let s2 = Temp.new_temp ()
let s3 = Temp.new_temp ()
let s4 = Temp.new_temp ()
let s5 = Temp.new_temp ()
let s6 = Temp.new_temp ()
let s7 = Temp.new_temp ()
let t0 = Temp.new_temp ()
let t1 = Temp.new_temp ()
let t2 = Temp.new_temp ()
let t3 = Temp.new_temp ()
let t4 = Temp.new_temp ()
let t5 = Temp.new_temp ()
let t6 = Temp.new_temp ()
let t7 = Temp.new_temp ()
let t8 = Temp.new_temp ()
let t9 = Temp.new_temp ()
    
let rv = v0 (* use only one of the return value registers for now *)

let specialregs =
  [fp; ra; sp; zero; gp; v0; v1; at; k0; k1]
  
let argregs = [a0; a1; a2; a3]
  
let calleesaves =
  [s0; s1; s2; s3; s4; s5; s6; s7]
    
let callersaves =
  [t0; t1; t2; t3; t4; t5; t6; t7; t8; t9]

let temp_map =
  Temp.Table.(
    empty |>
    add fp "$fp" |>
    add sp "$sp" |>
    add rv "$v0" |>
    add ra "$ra" |>
    add zero "$zero" |>
    add gp "$gp" |>
    add v0 "$v0" |>
    add v1 "$v1" |>
    add at "$at" |>
    add k0 "$k0" |>
    add k1 "$k1" |>
    add a0 "$a0" |>
    add a1 "$a1" |>
    add a2 "$a2" |>
    add a3 "$a3" |>
    add s0 "$s0" |>
    add s1 "$s1" |>
    add s2 "$s2" |>
    add s3 "$s3" |>
    add s4 "$s4" |>
    add s5 "$s5" |>
    add s6 "$s6" |>
    add s7 "$s7" |>
    add t0 "$t0" |>
    add t1 "$t1" |>
    add t2 "$t2" |>
    add t3 "$t3" |>
    add t4 "$t4" |>
    add t5 "$t5" |>
    add t6 "$t6" |>
    add t7 "$t7" |>
    add t8 "$t8" |>
    add t9 "$t9")

let alloc_local ({allocated; _} as frame) escape =
  if escape then
    let access = InFrame (-allocated * word_size) in
      frame.allocated <- frame.allocated + 1;
      access
  else
    InReg (Temp.new_temp ())

let new_frame name formals =
  let rec seq = function
    [] -> raise (Failure "seq")
    | [e] -> e
    | h::t -> Tree.SEQ (h, seq t) in
  let rec alloc_formals i allocated viewshift accesses = function
      [] -> (allocated, seq (List.rev viewshift), List.rev accesses)
    | formal :: formals ->
        let incoming =
            if i < List.length argregs then
              Tree.TEMP (List.nth argregs i)
            else
              let offset = (i - List.length argregs + 1) * word_size in
                Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.CONST offset, Tree.TEMP fp)) in
          if formal then
            let offset = -allocated * word_size in
            let access = InFrame offset in
            let instr =
              Tree.MOVE (Tree.MEM (Tree.BINOP (Tree.PLUS, Tree.CONST offset, Tree.TEMP fp)),
                         incoming) in
              alloc_formals (i + 1) (allocated + 1) (instr :: viewshift) (access :: accesses) formals
          else
            let temp = Temp.new_temp () in
            let access = InReg temp in
            let instr = Tree.MOVE (Tree.TEMP temp, incoming) in
              alloc_formals (i + 1) allocated (instr :: viewshift) (access :: accesses) formals in
  let (allocated, viewshift, formals) = alloc_formals 0 0 [] [] formals in
    {name; allocated; viewshift; formals}

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
let proc_entry_exit1 {viewshift; _} body =
  Tree.SEQ (viewshift, body)

let proc_entry_exit2 frame body =
  body @ [Assem.OPER {Assem.
                       assem = "";
                       src = [zero; ra; sp] @ calleesaves;
                       dst = []; jump = Some []}]

let proc_entry_exit3 {name; _} body =
  let prolog = "PROCEDURE " ^ Temp.string_of_label name ^ "\n" in
  let epilog = "END " ^ Temp.string_of_label name ^ "\n" in
    (prolog, body, epilog)

(* TODO *)
let string label s = ""
