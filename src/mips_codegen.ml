module A = Assem
module T = Tree

let calldefs = Frame.rv :: Frame.ra :: Frame.callersaves @ Frame.argregs

let codegen frame stm =
  let alist = ref [] in
  let emit instr = alist := instr :: !alist in
  let result f = let t = Temp.new_temp() in f t; t in
  
  let rec munch_args i = function
      [] -> []
    | arg :: args when i < List.length Frame.argregs ->
        let temp = munch_exp arg in
        let argreg = List.nth Frame.argregs i in
          emit (A.MOVE {
              A.assem = "move 'd0, 's0";
              src = temp;
              dst = argreg});
          argreg :: munch_args (i + 1) args
    | args ->
        List.mapi (fun i arg ->
            let temp = munch_exp arg in
              emit (A.OPER {
                  A.assem = "sw 's0, " ^ string_of_int (Frame.word_size * i) ^ "('s1)";
                  src = [temp; Frame.sp]; dst = []; jump = None});
              temp)
          args

  and munch_stm = function
      T.SEQ (a, b) -> munch_stm a; munch_stm b
    | T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)), e2) ->
        emit (A.OPER {
            A.assem = "sw 's0, " ^ string_of_int i ^ "('s1)";
            src = [munch_exp e2; munch_exp e1]; dst = []; jump = None})
    | T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)), e2) ->
        emit (A.OPER {
            A.assem = "sw 's0, " ^ string_of_int i ^ "('s1)";
            src = [munch_exp e2; munch_exp e1]; dst = []; jump = None})
    | T.MOVE (T.MEM (T.CONST i), e2) ->
        emit (A.OPER {
            A.assem = "sw 's0, " ^ string_of_int i;
            src = [munch_exp e2]; dst = []; jump = None})
    | T.MOVE (T.MEM (e1), e2) ->
        emit (A.OPER {
            A.assem = "sw 's1, ('s0)";
            src = [munch_exp e2; munch_exp e1]; dst = []; jump = None})
    | T.LABEL lab ->
        emit (A.LABEL {A.assem = Temp.string_of_label lab ^ ":"; lab = lab})
    | T.EXP (T.CALL (T.NAME lab, args)) ->
        emit (A.OPER {
            A.assem = "jal " ^ Temp.string_of_label lab;
            src = munch_args 0 args;
            dst = calldefs;
            jump =None})
    | T.MOVE (T.TEMP t, T.CALL (T.NAME lab, args)) ->
        emit (A.OPER {
            A.assem = "jal " ^ Temp.string_of_label lab;
            src = munch_args 0 args;
            dst = calldefs;
            jump =None});
        emit (A.MOVE {
            A.assem = "move 'd0, 's0";
            src = Frame.rv;
            dst = t})
    | T.MOVE (T.TEMP t, e1) ->
        emit (A.MOVE {
            A.assem = "move 'd0, 's0";
            src = munch_exp e1;
            dst = t})
    | T.JUMP (T.NAME lab, labs) when labs = [lab] ->
        emit (A.OPER {
            A.assem = "j 'j0";
            src = []; dst = [];
            jump = Some labs})
    | T.JUMP (e, labs) ->
        emit (A.OPER {
            A.assem = "jr 's0";
            src = [munch_exp e]; dst = [];
            jump = Some labs})
    | T.CJUMP (relop, e1, e2, truelab, falselab) ->
        let op2assem = function
            T.EQ -> "beq"
          | T.NE -> "bne"
          | T.LT -> "blt"
          | T.GT -> "bgt"
          | T.LE -> "ble"
          | T.GE -> "bge"
          | T.ULT -> "bltu"
          | T.ULE -> "bleu"
          | T.UGT -> "bgtu"
          | T.UGE -> "bgeu" in
          emit (A.OPER {
              A.assem = op2assem relop ^ " 's0, 's1, 'j0";
              src = [munch_exp e1; munch_exp e2];
              dst = [];
              jump = Some [truelab; falselab]})
            
  and  munch_exp = function
      T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)) ->
        result (fun r -> emit (A.OPER {A.
            assem = "lw 'd0, " ^ string_of_int i ^ "('s0)";
            src = [munch_exp e1]; dst = [r]; jump = None}))
    | T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)) ->
        result (fun r -> emit (A.OPER {A.
            assem = "lw 'd0, " ^ string_of_int i ^ "('s0)";
            src = [munch_exp e1]; dst = [r]; jump = None}))
    | T.MEM (T.CONST i) ->
        result (fun r -> emit (A.OPER {A.
            assem = "lw 'd0, " ^ string_of_int i;
            src =[]; dst = [r]; jump = None}))
    | T.MEM (e1) ->
        result (fun r -> emit (A.OPER {A.
            assem = "lw 'd0, 's0";
            src = [munch_exp e1]; dst = [r]; jump = None}))
    | T.BINOP (T.PLUS, e1, T.CONST i) ->
        result (fun r -> emit (A.OPER {A.
            assem = "addi 'd0, 's0, " ^ string_of_int i;
            src = [munch_exp e1]; dst = [r]; jump = None}))
    | T.BINOP (T.PLUS, T.CONST i, e1) ->
        result (fun r -> emit (A.OPER {A.
            assem = "addi 'd0, 's0, " ^ string_of_int i;
            src = [munch_exp e1]; dst = [r]; jump = None}))
    | T.BINOP (T.PLUS, e1, e2) ->
        result (fun r -> emit (A.OPER {A.
            assem = "add 'd0, 's0, 's1";
            src = [munch_exp e1; munch_exp e2]; dst = [r]; jump = None}))
    | T.BINOP (T.MINUS, e1, e2) ->
        result (fun r -> emit (A.OPER {A.
            assem = "sub 'd0, 's0, 's1";
            src = [munch_exp e1; munch_exp e2]; dst = [r]; jump = None}))
    | T.BINOP (T.MUL, e1, e2) ->
        result (fun r -> emit (A.OPER {A.
            assem = "mulo 'd0, 's0, 's1";
            src = [munch_exp e1; munch_exp e2]; dst = [r]; jump = None}))
    | T.BINOP (T.DIV, e1, e2) ->
        result (fun r -> emit (A.OPER {A.
            assem = "div 'd0, 's0, 's1";
            src = [munch_exp e1; munch_exp e2]; dst = [r]; jump = None}))
    | T.CONST i ->
        result (fun r -> emit (A.OPER {A.
            assem = "li 'd0, " ^ string_of_int i;
            src = []; dst = [r]; jump = None}))
    | T.NAME lab ->
        result (fun r -> emit (A.OPER {A.
            assem = "la 'd0, " ^ Temp.string_of_label lab;
            src = []; dst = [r]; jump = None}))
    | T.TEMP t -> t
      
    (* Should have already been translated into if-exp in the parser *)
    | T.BINOP (T.AND, _, _) | T.BINOP (T.OR, _, _) ->raise (Failure "codegen: binop logical")

    (* Not yet implemented in the complier *)
    | T.BINOP (T.LSHIFT, _, _) | T.BINOP (T.RSHIFT, _, _) |
      T.BINOP (T.ARSHIFT, _, _) | T.BINOP (T.XOR, _, _) ->raise (Failure "codegen: binop shift")

    (* Shouldn't be in a canonical tree *)
    | T.ESEQ _ -> raise (Failure "codegen: eseq")

    (* Shouldn't be in a canonical tree *)
    | T.CALL _ -> raise (Failure "codegen: call") in

    munch_stm stm;
    List.rev !alist
