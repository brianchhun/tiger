module T = Tree

(* Translated from Olin Shivers' canonicaliser (see misc/olincanon1.sml and
 * misc/olincanon2.sml) *)
let linearize stm0 =

  let nop = T.EXP (T.CONST 0) in

  let ( % ) x y =
    match x, y with
      T.EXP (T.CONST _), _ -> y
    | _, T.EXP (T.CONST _) -> x
    | _ -> T.SEQ (x, y) in

  let rec stmtall f = function
    | T.SEQ (a, b) -> stmtall f a && stmtall f b
    | s -> f s in

  let rec commutes stmt e =
    match stmt, e with
      _, T.NAME _ -> true
    | _, T.CONST _ -> true
    | stmt, T.BINOP (_, e1, e2) ->
        commutes stmt e1 && commutes stmt e2
    | stmt, T.TEMP t ->
        stmtall (function
            | T.JUMP _ -> false
            | T.CJUMP _ -> false
            | T.EXP (T.CALL _) -> true
            | T.MOVE (T.TEMP t', _) -> not (t = t')
            | T.MOVE _ -> true
            | T.EXP _ -> true
            | _ -> raise (Failure "commutes")
          )
          stmt
    | stmt, T.MEM _ ->
        stmtall (function
            | T.MOVE (T.MEM _, _) -> false
            | T.JUMP _ -> false
            | T.CJUMP _ -> false
            | T.EXP (T.CALL _) -> false
            | T.MOVE (_, T.CALL _) -> false
            | _ -> true)
          stmt
    | _ -> raise (Failure "commutes")
  in

  let rec do_exps = function
      [] -> (nop, [])
    | h::t ->
        let (stm1', e') = do_exp h in
        let (stm2', el) = do_exps t in
          if commutes stm2' e' then
            (stm1' % stm2', e' :: el)
          else
            let t = T.TEMP (Temp.new_temp ()) in
              (stm1' % T.MOVE (t, e') % stm2', t :: el)

  and do_stm = function
      T.SEQ (a, b) -> 
        do_stm a % do_stm b
    | T.JUMP (e, labs) ->   
        let (stm', e') = do_exp e in
          stm' % T.JUMP (e', labs)
    | T.CJUMP (p, a, b, t, f) ->
        let (stm', [a'; b']) = do_exps [a; b] in
          stm' % T.CJUMP (p, a', b', t, f)
    | T.MOVE (T.TEMP t, b) ->
        let (stm', rv) = do_rval b in
          stm' % T.MOVE (T.TEMP t, rv)
    | T.MOVE (T.MEM e, b) ->
        let (stm1', e') = do_exp e in
        let (stm2', rv) = do_rval b in
          if commutes stm2' e' then
            stm1' % stm2' % T.MOVE (T.MEM e', rv)
          else
            let t = T.TEMP (Temp.new_temp ()) in
              stm1' % T.MOVE (t, e') % stm2' % T.MOVE (T.MEM t, rv)
    | T.EXP e ->
        let (stm', _) = do_exp e in
          stm'
    | T.LABEL _ as label ->
        label

  and do_exp = function
      T.BINOP (p, a, b) ->
        let (stm', [a'; b']) = do_exps [a; b] in
            (stm', T.BINOP (p, a', b'))
    | T.MEM a ->
        let (stm', a') = do_exp a in
          (stm', T.MEM a')
    | T.ESEQ (s, e) ->
        let stm1' = do_stm s in
        let (stm2', e') = do_exp e in
          (stm1' % stm2', e')
    | T.CALL (f, args) ->
        let (stm', f' :: args') = do_exps (f :: args) in
        let t = T.TEMP (Temp.new_temp ()) in
          (stm' % (T.MOVE (t, T.CALL (f', args'))), t)
    | T.TEMP _ | T.CONST _ | T.NAME _ as other ->
        (nop, other)

  and do_rval = function
      T.ESEQ (s, e) -> 
        let stm1' = do_stm s in
        let (stm2', rv) = do_rval e in
          (stm1' % stm2', rv)
    | T.CALL (f, args) ->
        let (stm', f' :: args') = do_exps (f :: args) in
          (stm', T.CALL (f', args'))
    | exp -> do_exp exp in

  let rec linear stm l =
    match stm with
      T.SEQ (a, b) ->
        linear a (linear b l)
    | s -> s :: l 

  in linear (do_stm stm0) []

let basic_blocks stms =
  let donelab = Temp.new_label () in
  let rec blocks stms blist =
    match stms with
      T.LABEL _ as h :: l ->
        let rec next stms thisblock =
          match stms with
            T.JUMP _ as h :: l ->
              end_block l (h :: thisblock)
          | T.CJUMP _ as h :: l ->
              end_block l (h :: thisblock)
          | T.LABEL lab :: _ ->
              next ((T.JUMP (T.NAME lab, [lab])) :: stms) thisblock
          | h :: l ->
              next l (h :: thisblock)
          | [] ->
              next [T.JUMP (T.NAME donelab, [donelab])] thisblock 
        and end_block stms thisblock =
          blocks stms (List.rev thisblock :: blist)
        in next l [h]
    | [] ->
        List.rev blist
    | _ ->
        blocks (T.LABEL (Temp.new_label ()) :: stms) blist

  in (blocks stms [], donelab)

let enter_block block table =
  match block with
    T.LABEL s :: _ ->
      Symbol.enter s block table
  | _ -> table

let rec split_last = function
    [] -> raise (Failure "split_last")
  | [x] -> ([], x)
  | h :: t -> let (t', last) = split_last t in (h::t', last) 

let rec trace table (T.LABEL lab :: _ as b) rest =
  let table = Symbol.enter lab [] table in
    match split_last b with
      most, T.JUMP (T.NAME lab, _) ->
        begin
          match Symbol.look lab table with
            Some (_ :: _ as b') ->
              most @ trace table b' rest
          | _ ->
              b @ get_next table rest
        end
    | most, T.CJUMP (op, x, y, t, f) ->
        begin
          match Symbol.look t table, Symbol.look f table with
            _, Some (_ :: _ as b') ->
              b @ trace table b' rest
          | Some (_ :: _ as b'), _ ->
              most @ [T.CJUMP (T.not_relop op, x, y, f, t)] @ trace table b' rest
          | _ ->
              let f' = Temp.new_label () in
                most @ [T.CJUMP (op, x, y, t, f');
                        T.LABEL f';
                        T.JUMP (T.NAME f, [f])] @ get_next table rest
        end
    | most, T.JUMP _ ->
        b @ get_next table rest

and get_next table = function
    [] -> []
  | (T.LABEL lab :: _ as b) :: rest ->
      begin
        match Symbol.look lab table with
          Some (_ :: _) -> trace table b rest
        | _ -> get_next table rest
      end

let trace_schedule (blocks, donelab) =
  let table = List.fold_right enter_block blocks Symbol.empty in
    get_next table blocks @ [T.LABEL donelab]
