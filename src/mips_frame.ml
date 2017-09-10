type access =
    InFrame of int
  | InReg of Temp.temp

type frame =
  {name: Temp.label;
   formals: access list;
   fpaccess: access;
   mutable allocated: int;
   viewshift: Tree.stm;
   mutable max_outgoing: int}

type frag =
    STRING of Temp.label * string
  | PROC of Tree.stm * frame

let word_size = 4
let min_frame_size = 24

type register = string

let registers = [
  "$fp";
  "$sp";
  "$ra";
  "$zero";
  "$gp";
  "$v0";
  "$v1";
  "$at";
  "$k0";
  "$k1";
  "$a0";
  "$a1";
  "$a2";
  "$a3";
  "$s0";
  "$s1";
  "$s2";
  "$s3";
  "$s4";
  "$s5";
  "$s6";
  "$s7";
  "$t0";
  "$t1";
  "$t2";
  "$t3";
  "$t4";
  "$t5";
  "$t6";
  "$t7";
  "$t8";
  "$t9"
]

let zero = Temp.new_temp () (* always 0 *)
let gp = Temp.new_temp ()   (* global pointer *)
let at = Temp.new_temp ()   (* reserved *)
let k0 = Temp.new_temp ()   (* reserved *)
let k1 = Temp.new_temp ()   (* reserved *)
let fp = Temp.new_temp ()   (* frame pointer *)
let sp = Temp.new_temp ()   (* stack pointer *)
let ra = Temp.new_temp ()   (* return address *)
let v0 = Temp.new_temp ()   (* return value *)
let v1 = Temp.new_temp ()   (* return value *)
let a0 = Temp.new_temp ()   (* argument *)
let a1 = Temp.new_temp ()   (* argument *)
let a2 = Temp.new_temp ()   (* argument *)
let a3 = Temp.new_temp ()   (* argument *)
let s0 = Temp.new_temp ()   (* callee-saved *)
let s1 = Temp.new_temp ()   (* callee-saved *)
let s2 = Temp.new_temp ()   (* callee-saved *)
let s3 = Temp.new_temp ()   (* callee-saved *)
let s4 = Temp.new_temp ()   (* callee-saved *)
let s5 = Temp.new_temp ()   (* callee-saved *)
let s6 = Temp.new_temp ()   (* callee-saved *)
let s7 = Temp.new_temp ()   (* callee-saved *)
let t0 = Temp.new_temp ()   (* caller-saved *)
let t1 = Temp.new_temp ()   (* caller-saved *)
let t2 = Temp.new_temp ()   (* caller-saved *)
let t3 = Temp.new_temp ()   (* caller-saved *)
let t4 = Temp.new_temp ()   (* caller-saved *)
let t5 = Temp.new_temp ()   (* caller-saved *)
let t6 = Temp.new_temp ()   (* caller-saved *)
let t7 = Temp.new_temp ()   (* caller-saved *)
let t8 = Temp.new_temp ()   (* caller-saved *)
let t9 = Temp.new_temp ()   (* caller-saved *)
    
let rv = v0 (* use only one of the return value registers for now *)

let specialregs =
  [rv; ra; fp; sp; zero; gp; at; k0; k1]
  
let argregs = [a0; a1; a2; a3]
  
let calleesaves =
  [s0; s1; s2; s3; s4; s5; s6; s7]
    
let callersaves =
  [t0; t1; t2; t3; t4; t5; t6; t7; t8; t9; v1]

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

let string_of_temp t =
  try
    Temp.Table.find t temp_map
  with Not_found ->
    Temp.string_of_temp t

let alloc_local ({allocated; _} as frame) escape =
  if escape then
    let access = InFrame (-allocated * word_size) in
      frame.allocated <- frame.allocated + 1;
      access
  else
    InReg (Temp.new_temp ())

let rec seq = function
    [] -> raise (Failure "seq")
  | [e] -> e
  | h::t -> Tree.SEQ (h, seq t)

let exp access fp =
  match access with
    InFrame k ->
      Tree.MEM (Tree.BINOP (Tree.PLUS, fp, Tree.CONST k))
  | InReg t ->
      Tree.TEMP t

let new_frame name formals =
  let exp access = exp access (Tree.TEMP fp) in
  let rec alloc_formals i allocated viewshift accesses = function
      [] -> (allocated, seq (List.rev viewshift), List.rev accesses)
    | formal :: formals ->
        let incoming =
          if i < List.length argregs then InReg (List.nth argregs i)
          else InFrame ((i - List.length argregs + 1) * word_size) in
        let access =
          if formal then InFrame (-allocated * word_size) else InReg (Temp.new_temp ()) in
        let instr = Tree.MOVE (exp access, exp incoming) in
          alloc_formals (i + 1) (if formal then allocated + 1 else allocated) (instr :: viewshift) (access :: accesses) formals in
  let (allocated, viewshift, formals) = alloc_formals 0 0 [] [] formals in
    {name; allocated=allocated+1; viewshift; formals; max_outgoing=0; fpaccess=(InFrame (-allocated * word_size))}

let name {name; _} = name
let formals {formals; _} = formals

let external_call name args =
  Tree.CALL (Tree.NAME (Temp.named_label name), args)

let proc_entry_exit1 ({viewshift; _} as frame) body =
  let (save, restore) =
    List.split
      (List.map
         (fun t ->
            let exp = exp (alloc_local frame false) (Tree.TEMP fp) in
              (Tree.MOVE (exp, Tree.TEMP t), Tree.MOVE (Tree.TEMP t, exp)))
         (calleesaves @ [ra])) in
    seq [seq save; viewshift; body; seq restore]

let proc_entry_exit2 frame body =
  let max_outgoing =
    List.fold_left
      (fun max instr ->
         match instr with
           Assem.OPER {Assem.assem; src=[_; s1]; _} when s1 == sp ->
             if Str.string_match (Str.regexp "sw 's0, \\([0-9]+\\)('s1)") assem 0 then
               let m = int_of_string (Str.matched_group 1 assem) / 4 + 1 in
                 if m > max then m else max
             else max
         | _ -> max)
      0
      body in
    frame.max_outgoing <- max_outgoing;
    body @ [Assem.OPER {Assem.
                         assem = "";
                         src = specialregs @ calleesaves;
                         dst = []; jump = None}]

let proc_entry_exit3 {name; allocated; max_outgoing; fpaccess; _} body =
  let fpoffset = match fpaccess with InFrame k -> k in
  let fs =
    let pad fs = if fs mod (word_size * 2) = 0 then fs else fs + word_size in
    let n = (allocated + max_outgoing) * word_size in
      if n < min_frame_size then min_frame_size else pad n in
  let prologue = [
    Assem.LABEL {
      Assem.assem = Temp.string_of_label name ^ ":";
      lab = name};
    Assem.OPER {
      Assem.assem = "subu 'd0, 's0, " ^ string_of_int fs;
      src = [sp]; dst = [sp];
      jump = None};
     Assem.OPER {
       Assem.assem = "sw 's0, " ^ string_of_int (fs - word_size + fpoffset) ^ "('s1)";
       src = [fp; sp]; dst = [];
       jump = None};
     Assem.OPER {
       Assem.assem = "addiu 'd0, 's0, " ^ string_of_int (fs - word_size);
       src = [sp]; dst = [fp];
       jump = None}] in 
  let epilogue = [Assem.OPER {
      Assem.assem = "lw 'd0, " ^ string_of_int (fs - word_size + fpoffset) ^ "('s0)";
       src = [sp]; dst = [fp];
       jump = None};
     Assem.OPER {
      Assem.assem = "addiu 's0, 'd0, " ^ string_of_int fs;
      src = [sp]; dst = [sp];
      jump = None};
     Assem.OPER {
       Assem.assem = "jr 's0";
       src = [ra]; dst =[];
       jump = None}] in
  let mk_string instrs = List.fold_left (fun s instr -> s ^ Assem.format string_of_temp instr ^ "\n") "" instrs in
    (".text\n" ^ mk_string prologue, body, mk_string epilogue)

let string label s =
  let size = String.length s in
  let bytes = Array.make (size+4) 0 in
    Array.set bytes 0 (size land 0x000000ff);
    Array.set bytes 1 ((size land 0x0000ff00) lsr 8);
    Array.set bytes 2 ((size land 0x00ff0000) lsr 16);
    Array.set bytes 3 ((size land 0xff000000) lsr 24);
    String.iteri (fun i c -> Array.set bytes (i+4) (Char.code c)) s;
  let bytestring = Array.fold_left (fun bs b -> bs ^ string_of_int b ^ " ") "" bytes in
  ".data\n.align 2\n" ^ (Temp.string_of_label label) ^ ":\n\t.byte " ^ bytestring ^ "\n.text\n"

let string_of_register r = r
