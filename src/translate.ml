module T = Tree

type level =
    Outermost
  | Level of level * Frame.frame * unit ref

type access = level * Frame.access
                        
type exp =
    Ex of T.exp
  | Nx of T.stm
  | Cx of (Temp.label -> Temp.label -> T.stm)

type frag = Frame.frag

let frags = ref [] 

let get_result () = !frags

let outermost = Outermost

let new_level parent name formals =
  (* Include static link in formals *)
  let formals' = true :: formals in
  let frame = Frame.new_frame name formals' in
    Level (parent, frame, ref ())

let formals = function
    Outermost -> []
  | Level (_, frame, _) as level ->
      (* Remove static link from formals *)
      let formals' = (List.tl (Frame.formals frame)) in
        List.map (fun access -> (level, access)) formals'

let alloc_local level escape =
  match level with
    Outermost -> raise (Failure "alloc_local")
  | Level (_, frame, _) as level ->
      let access = Frame.alloc_local frame escape in
        (level, access)

let rec seq = function
    [] -> raise (Failure "seq")
  | [e] -> e
  | h::t -> T.SEQ (h, seq t)

let un_ex = function
    Ex e -> e
  | Cx genstm ->
      let r = Temp.new_temp () in
      let t = Temp.new_label () in
      let f = Temp.new_label () in
        T.ESEQ (
          seq [
            T.MOVE (T.TEMP r, T.CONST 0);
            genstm t f;
            T.LABEL t;
            T.MOVE (T.TEMP r, T.CONST 1);
            T.LABEL f;
          ],
          T.TEMP r)
  | Nx s ->
      T.ESEQ(s, T.CONST 0)

let un_cx = function
    Ex (T.CONST 0) ->
      (fun _ f -> T.JUMP (T.NAME f, [f]))
  | Ex (T.CONST 1) ->
      (fun t _ -> T.JUMP (T.NAME t, [t]))
  | Ex e ->
      (fun t f -> T.CJUMP (T.NE, e, T.CONST 0, t, f))
  | Cx genstm -> genstm
  | Nx _ -> raise (Failure "un_cx")

let un_nx = function
    Ex e -> T.EXP e
  | Cx genstm ->
      let l = Temp.new_label () in
        T.SEQ (genstm l l, T.LABEL l)
  | Nx stm -> stm

let unit_exp = Nx (T.EXP (T.CONST 0))

let simple_var (level, access) level' =
  let rec find_fp level level' fp =
    match level, level' with
      Level (_, _, ref), Level (_, _, ref') when ref == ref' ->
        fp
    | Level _ , Level (parent, frame, _) ->
        (* Follow static link *)
        let fp' = Frame.exp (List.hd (Frame.formals frame)) fp in
          find_fp level parent fp'
    | _ -> raise (Failure "find_fp") in
  let fp = find_fp level level' Frame.fp in
    Ex (Frame.exp access fp)

let field_var r i =
  Ex (T.MEM (
      T.BINOP (T.PLUS,
               (un_ex r),
               T.BINOP (T.MUL, T.CONST i, T.CONST Frame.word_size))))

let subscript_var a i =
  Ex (T.MEM (
      T.BINOP (T.PLUS,
               (un_ex a),
               T.BINOP (T.MUL, (un_ex i), T.CONST Frame.word_size))))

let nil_exp = Ex (T.CONST 0)
let int_exp i = Ex (T.CONST i)

let plus_op_exp a b = Ex (T.BINOP (T.PLUS, un_ex a, un_ex b))
let minus_op_exp a b = Ex (T.BINOP (T.MINUS, un_ex a, un_ex b))
let mul_op_exp a b = Ex (T.BINOP (T.MUL, un_ex a, un_ex b))
let div_op_exp a b = Ex (T.BINOP (T.DIV, un_ex a, un_ex b))

let eq_op_exp a b = Cx (fun t f -> T.CJUMP (T.EQ, un_ex a, un_ex b, t, f))
let ne_op_exp a b = Cx (fun t f -> T.CJUMP (T.NE, un_ex a, un_ex b, t, f))

let string_eq_op_exp a b =
  Cx (fun t f ->
      let r = Frame.external_call "stringEquals" [un_ex a; un_ex b] in
        T.CJUMP (T.EQ, r, T.CONST 1, t, f))

let string_ne_op_exp a b =
  Cx (fun t f -> let genstm = un_cx (string_eq_op_exp a b) in genstm f t)

let lt_op_exp a b = Cx (fun t f -> T.CJUMP (T.LT, un_ex a, un_ex b, t, f))
let le_op_exp a b = Cx (fun t f -> T.CJUMP (T.LE, un_ex a, un_ex b, t, f))
let gt_op_exp a b = Cx (fun t f -> T.CJUMP (T.GT, un_ex a, un_ex b, t, f))
let ge_op_exp a b = Cx (fun t f -> T.CJUMP (T.GE, un_ex a, un_ex b, t, f))

let if_exp2 test conseq =
  let t = Temp.new_label () in
  let f = Temp.new_label () in
  let genstm = un_cx test in
    Nx (seq [
        genstm t f;
        T.LABEL t;
        un_nx conseq;
        T.LABEL f
      ])

let if_exp3 test conseq alt =
  let t = Temp.new_label () in
  let f = Temp.new_label () in
  let join = Temp.new_label () in
  let genstm = un_cx test in
    match conseq, alt with
      Nx _, Nx _ ->
        Nx (seq [
            genstm t f;
            T.LABEL t;
            un_nx conseq;
            T.JUMP (T.NAME join, [join]);
            T.LABEL f;
            un_nx alt;
            T.LABEL join;
          ])
    | Cx genstm', Cx genstm'' ->
        let r = Temp.new_temp () in
        let z = Temp.new_label () in
        let z' = Temp.new_label () in
          Ex (T.ESEQ (
              seq [
                genstm z z';
                T.LABEL z;
                genstm' t f;
                T.LABEL z';
                genstm'' t f;
                T.LABEL t;
                T.MOVE (T.TEMP r, T.CONST 1);
                T.JUMP (T.NAME join, [join]);
                T.LABEL f;
                T.MOVE (T.TEMP r, T.CONST 0);
                T.LABEL join;
              ],
              T.TEMP r))
    | Cx genstm', _ ->
        let r = Temp.new_temp () in
        let z = Temp.new_label () in
          Ex (T.ESEQ (
              seq [
                genstm z f;
                T.LABEL z;
                genstm' t f;
                T.LABEL t;
                T.MOVE (T.TEMP r, T.CONST 1);
                T.JUMP (T.NAME join, [join]);
                T.LABEL f;
                T.MOVE (T.TEMP r, un_ex alt);
                T.LABEL join;
              ],
              T.TEMP r))
    | _, Cx genstm' ->
        let r = Temp.new_temp () in
        let z = Temp.new_label () in
          Ex (T.ESEQ (
              seq [
                genstm t z;
                T.LABEL z;
                genstm' t f;
                T.LABEL t;
                T.MOVE (T.TEMP r, un_ex conseq);
                T.JUMP (T.NAME join, [join]);
                T.LABEL f;
                T.MOVE (T.TEMP r, T.CONST 0);
                T.LABEL join;
              ],
              T.TEMP r))
    | _ ->
        let r = Temp.new_temp () in
          Ex (T.ESEQ (
              seq [
                genstm t f;
                T.LABEL t;
                T.MOVE (T.TEMP r, un_ex conseq);
                T.JUMP (T.NAME join, [join]);
                T.LABEL f;
                T.MOVE (T.TEMP r, un_ex alt);
                T.LABEL join;
              ],
              T.TEMP r))

let string_exp lit =
  let label = Temp.new_label () in
  let frag = Frame.STRING (label, lit) in
    frags := frag :: !frags;
    Ex (T.NAME label)

let record_exp fields =
  let len = List.length fields in
  let r = Temp.new_temp () in
    Ex (T.ESEQ (
        seq (
          T.MOVE (T.TEMP r, Frame.external_call "malloc" [T.CONST (len * Frame.word_size)]) ::
          List.mapi
            (fun i e -> T.MOVE (T.MEM (T.BINOP (T.PLUS, T.TEMP r, T.CONST (i * Frame.word_size))), un_ex e))
            fields
        ),
        T.TEMP r))

let array_exp len init =
  let r = Temp.new_temp () in
    Ex (T.ESEQ (
        T.MOVE (T.TEMP r, Frame.external_call "initArray" [un_ex len; un_ex init]),
        T.TEMP r))

let rec seq_exp = function
    [] -> unit_exp
  | [e] -> Ex (un_ex e)
  | h::t -> Ex (T.ESEQ (un_nx h, un_ex (seq_exp t)))

let while_exp test body donelab  =
  let testlab = Temp.new_label () in
  let bodylab = Temp.new_label () in
  let genstm = un_cx test in
    Nx (seq [
        T.LABEL testlab;
        genstm bodylab donelab;
        T.LABEL bodylab;
        un_nx body;
        T.JUMP (T.NAME testlab, [testlab]);
        T.LABEL donelab
      ])

let break_exp donelab =
  Nx (T.JUMP (T.NAME donelab, [donelab]))

let call_exp flevel level label args =
  let rec depth = function
      Outermost -> 0
    | Level (p, _, _) -> 1 + depth p in
  let fdepth = depth flevel in
  let rec find_sl level fp =
    let diff = fdepth - (depth level) in
      match level with
      | Level (p, f, _) ->
          if diff = 0 then
            (* Static link of current fp *)
            Frame.exp (List.hd (Frame.formals f)) fp
          else if diff = 1 then
            (* Current fp *)
            fp
          else if diff < 0 then
            (* Follow static link of current fp *)
            let sl = Frame.exp (List.hd (Frame.formals f)) fp in
              find_sl p sl
          else
            raise (Failure "find_sl")
      | Outermost ->
          raise (Failure "find_sl") in

  let sl = if flevel = Outermost then T.CONST 0 else find_sl level Frame.fp in
    Ex (T.CALL (T.NAME label, sl :: List.map un_ex args))

let assign_exp var exp =
  Nx (T.MOVE (un_ex var, un_ex exp))

let let_exp assigns body =
  seq_exp (assigns @ [body])

let proc_entry_exit level body =
  let body' = T.MOVE (T.TEMP Frame.rv, (un_ex body)) in
    match level with
      Outermost ->
        raise (Failure ("proc_entry_exit"))
    | Level (_, f, _) ->
        let body'' = Frame.proc_entry_exit1 f body' in
          frags := !frags @ [Frame.PROC (body'', f)]

let print_tree t = Printtree.printtree (stdout, un_nx t)
let print_frags l =
  let print_frag =  function
    | Frame.STRING (l, s) ->
        print_string ("STRING(" ^ Temp.string_of_label l ^ ", " ^ s ^ ")\n")
    | Frame.PROC (stm, _) ->
        print_string ("PROC(\n");  print_tree (Ex (T.ESEQ(stm, T.CONST 0))); print_string ")\n"
  in 
    print_string ("Fragments: " ^ string_of_int (List.length l) ^ "\n");
    List.iter print_frag l
