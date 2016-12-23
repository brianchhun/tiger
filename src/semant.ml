module A = Absyn
module S = Symbol
module T = Types

type venv = Env.enventry Symbol.table
type tenv = Types.t Symbol.table
type expty = {exp: Translate.exp; ty: Types.t}
type decenv = {venv: venv; tenv: tenv; exps: Translate.exp list}

let check_expty ty1 {exp; ty=ty2} pos =
  match ty1, ty2 with
  | T.RECORD _ , T.NIL      -> ()
  | T.NIL      , T.RECORD _ -> ()
  | T.RECORD (_, unique1) , T.RECORD (_, unique2) ->
      if unique1 != unique2
      then Error_msg.error pos (Error_msg.Record_type_mismatch)
  | T.ARRAY (_, unique1)  , T.ARRAY (_, unique2)  ->
      if unique1 != unique2
      then Error_msg.error pos (Error_msg.Array_type_mismatch)
  | _ ->
      if ty1 <> ty2
      then Error_msg.error pos (Error_msg.Type_mismatch (T.string_of_ty ty1, T.string_of_ty ty2))

let checkint = check_expty T.INT

let checkunit = check_expty T.UNIT

let checkcomp lty rty pos =
  match lty, rty  with
  | T.RECORD _, T.NIL      -> ()
  | T.NIL     , T.RECORD _ -> ()
  | T.RECORD _, T.RECORD _ -> ()
  | T.ARRAY _ , T.ARRAY _  -> ()
  | T.INT     , T.INT      -> ()
  | T.STRING  , T.STRING   -> ()
  | _ -> Error_msg.error pos
           (Error_msg.Illegal_comparison (T.string_of_ty lty, T.string_of_ty rty))

let rec actual_ty = function
  | T.NAME (_, ty) ->
      begin
        match !ty with
          None -> Error_msg.impossible "name type without actual type"
        | Some ty' -> actual_ty ty'
      end
  | ty' -> ty'

let rec trans_var venv tenv break level = function
  | A.SimpleVar (id, pos) ->
      begin
        match S.look id venv with
          Some (Env.VarEntry (access, ty)) ->
            {exp = Translate.simple_var access level; ty}
        | _ ->
            Error_msg.error pos (Error_msg.Undefined_variable (S.name id));
            {exp = Translate.unit_exp; ty = T.INT}
      end
  | A.FieldVar (var, id, pos) ->
      let {exp = record_exp; ty = record_ty} = trans_var venv tenv break level var in
        begin
          match record_ty with
          | T.RECORD (fields, _) ->
              begin      
                try
                  let (i, (_, ty)) = List.find
                      (fun (_, (name, _)) -> name = id) 
                      (List.mapi (fun i e -> (i, e)) fields) in
                    {exp = Translate.field_var record_exp i; ty = actual_ty ty}
                with Not_found ->
                  Error_msg.error pos (Error_msg.Undefined_record_field (S.name id));
                  {exp = Translate.unit_exp; ty = T.INT}
              end
          | ty ->
              Error_msg.error pos (Error_msg.Type_mismatch ("record", T.string_of_ty ty));
              {exp = Translate.unit_exp; ty = T.INT}
        end
  | A.SubscriptVar (var, i, pos) ->
      let {exp = array_exp; ty = array_ty} = trans_var venv tenv break level var in
        begin
          match actual_ty array_ty with
          | T.ARRAY (elem_ty, _) ->
              let {exp = subscript_exp; ty = subscript_ty} as subscript_expty = trans_exp venv tenv break level i in
                checkint subscript_expty pos;
                let exp = Translate.subscript_var array_exp subscript_exp in
                let ty = actual_ty elem_ty in
                  {exp; ty}
          | ty ->
              Error_msg.error pos (Error_msg.Type_mismatch ("array", T.string_of_ty ty));
              {exp = Translate.unit_exp; ty = T.INT}
        end

and trans_exp venv tenv break level exp =
  let rec trexp = function
    | A.VarExp var ->
        trans_var venv tenv break level var
    | A.NilExp ->
        {exp = Translate.nil_exp; ty = T.NIL}
    | A.IntExp i ->
        {exp = Translate.int_exp i; ty = T.INT}
    | A.StringExp (str, _) ->
        {exp = Translate.string_exp str; ty = T.STRING}
    | A.CallExp (name, args, pos) ->
        begin
          match S.look name venv with
          | Some (Env.FunEntry (flevel, label, formal_tys, result_ty)) ->
              let numformals = List.length formal_tys in
              let numactuals = List.length args in
                if numformals <> numactuals then
                  begin
                    Error_msg.error pos (Error_msg.Arity_mismatch (numformals, numactuals));
                    {exp = Translate.unit_exp; ty = T.INT}
                  end
                else
                  let arg_exps = List.map2
                      (fun formal_ty arg ->
                         let {exp; _ } as expty = trexp arg in check_expty formal_ty expty pos; exp)
                      formal_tys
                      args in 
                    {exp = Translate.call_exp flevel level label arg_exps; ty = result_ty}
          | _ ->
              Error_msg.error pos (Error_msg.Undefined_function (S.name name));
              {exp = Translate.unit_exp; ty = T.INT}
        end
    | A.OpExp (l, op, r, pos) when op = A.EqOp || op = A.NeqOp ->
        let {exp = lexp; ty = lty} = trexp l in
        let {exp = rexp; ty = rty} = trexp r in
          checkcomp lty rty pos;
          let exp = match op, lty, rty with
              A.EqOp, T.STRING, T.STRING -> Translate.string_eq_op_exp lexp rexp
            | A.NeqOp, T.STRING, T.STRING -> Translate.string_ne_op_exp lexp rexp
            | A.EqOp, _, _ -> Translate.eq_op_exp lexp rexp
            | A.NeqOp, _, _ -> Translate.ne_op_exp lexp rexp
            | _ -> raise (Failure "trans_exp")
          in {exp; ty = T.INT}
    | A.OpExp (l, op, r, pos) ->
        let {exp = lexp; _} as lexpty = trexp l in
        let {exp = rexp; _} as rexpty = trexp r in
        checkint lexpty pos;
        checkint rexpty pos;
        let exp = match op with
            A.PlusOp -> Translate.plus_op_exp lexp rexp
          | A.MinusOp -> Translate.minus_op_exp lexp rexp
          | A.TimesOp -> Translate.mul_op_exp lexp rexp
          | A.DivideOp -> Translate.div_op_exp lexp rexp
          | A.LtOp -> Translate.lt_op_exp lexp rexp
          | A.LeOp -> Translate.le_op_exp lexp rexp
          | A.GtOp -> Translate.gt_op_exp lexp rexp
          | A.GeOp -> Translate.ge_op_exp lexp rexp
          | _ -> raise (Failure "trans_exp")
        in {exp; ty = T.INT}
    | A.RecordExp (fields, ty_id, pos) ->
        begin
          match S.look ty_id tenv with
          | Some record_ty ->
              begin
                match actual_ty record_ty with
                | T.RECORD (field_tys, _) as record_ty' ->
                    let field_exps = List.map2
                        (fun (name, ty) (name', exp, pos) ->
                           if name <> name' then
                             Error_msg.error pos (Error_msg.Record_field_mismatch (S.name name, S.name name'));
                           let {exp = field_exp; ty = field_ty} as field_expty = trexp exp in
                             check_expty (actual_ty ty) field_expty pos;
                             field_exp)
                        field_tys
                        fields
                    in {exp = Translate.record_exp field_exps; ty = record_ty'}
                | ty ->
                    Error_msg.error pos (Error_msg.Type_mismatch ("record", T.string_of_ty ty));
                    {exp = Translate.unit_exp; ty = T.RECORD ([], ref ())}
              end
          | None ->
              Error_msg.error pos (Error_msg.Undefined_record (S.name ty_id));
              {exp = Translate.unit_exp; ty = T.RECORD([], ref ())}
        end
    | A.SeqExp (exps) ->
        let (exps, ty) = List.fold_left
            (fun (exps, _) (e, _) -> let {exp; ty} = trexp e in (exps @ [exp], ty))
            ([], T.UNIT)
            exps in
          {exp = Translate.seq_exp exps; ty}
    | A.AssignExp (var, exp, pos) ->
        let {exp = var_exp; ty} = trans_var venv tenv break level var in
        let {exp = val_exp; _} as val_expty = trexp exp in
          check_expty ty val_expty pos;
          {exp = Translate.assign_exp var_exp val_exp; ty = T.UNIT}
    | A.IfExp (test, conseq, None, pos) ->
        let {exp = test_exp; _} as test_expty = trexp test in
        let {exp = conseq_exp; _} as conseq_expty = trexp conseq in
          checkint test_expty pos;
          checkunit conseq_expty pos;
          {exp = Translate.if_exp2 test_exp conseq_exp; ty = T.UNIT}
    | A.IfExp (test, conseq, Some alt, pos) ->
        let {exp = test_exp; _} as test_expty = trexp test in
        let {exp = conseq_exp; ty = conseq_ty} = trexp conseq in
        let {exp = alt_exp; _} as alt_expty = trexp alt in
          checkint test_expty pos;
          check_expty conseq_ty alt_expty pos;
          {exp = Translate.if_exp3 test_exp conseq_exp alt_exp; ty = conseq_ty}
    | A.WhileExp (test, body, pos) ->
        let done_label = Temp.new_label () in
        let {exp = test_exp; _ } as test_expty = trexp test in
        let {exp = body_exp; _ } as body_expty = trans_exp venv tenv (Some done_label) level body in
          checkint test_expty pos;
          checkunit body_expty pos;
          {exp = Translate.while_exp test_exp body_exp done_label; ty = T.UNIT}
    | A.ForExp (var, escape, lo, hi, body, pos) ->
        (*
            Rewrites the absyn into a let/while:

              let var i := lo
                  var limit := hi in
                if lo <= hi then
                  while 1 do
                    (body; if i < limit then i := i + 1 else break)
                  end

           TODO: How to suppress duplicate error message? test11.tig
        *)
        let limit_sym = Symbol.symbol "*limit*" in
        let lo_sym = Symbol.symbol "*lo*" in
        let hi_sym = Symbol.symbol "*hi*" in
        let decs = [A.VarDec {A.vardec_name = lo_sym;
                              vardec_escape = ref false;
                              vardec_ty = None;
                              vardec_init = lo;
                              vardec_pos = pos};
                    A.VarDec {A.vardec_name = hi_sym;
                              vardec_escape = ref false;
                              vardec_ty = None;
                              vardec_init = hi;
                              vardec_pos = pos};
                    A.VarDec {A.vardec_name = var;
                              vardec_escape = escape;
                              vardec_ty = None;
                              vardec_init = A.VarExp (A.SimpleVar (lo_sym, pos));
                              vardec_pos = pos};
                    A.VarDec {A.vardec_name = limit_sym;
                              vardec_escape = ref false;
                              vardec_ty = None;
                              vardec_init = A.VarExp (A.SimpleVar (hi_sym, pos));
                              vardec_pos = pos}] in
        let body' = A.IfExp (
            A.OpExp (A.VarExp (A.SimpleVar (lo_sym, pos)), A.LeOp, A.VarExp (A.SimpleVar (hi_sym, pos)), pos),
            A.WhileExp (
              A.IntExp 1,
              A.SeqExp [
                (body, pos);
                (A.IfExp (
                  A.OpExp (A.VarExp (A.SimpleVar (var, pos)), A.LtOp, A.VarExp (A.SimpleVar (limit_sym, pos)), pos),
                  A.AssignExp (A.SimpleVar (var, pos), A.OpExp (A.VarExp (A.SimpleVar (var, pos)), A.PlusOp, A.IntExp 1, pos), pos),
                  Some (A.BreakExp pos),
                  pos
                  ), pos)
              ],
              pos
            ),
            None,
            pos) in
          trexp (A.LetExp (decs, body', pos)) 
    | A.BreakExp pos ->
        begin
          match break with
            Some donelab ->
              {exp = Translate.break_exp donelab; ty = T.UNIT}
          | None ->
              Error_msg.error pos Error_msg.Illegal_break;
              {exp = Translate.unit_exp; ty = T.UNIT}
        end
    | A.LetExp (decs, body, _) ->
        let {venv = venv'; tenv = tenv'; exps = assign_exps} = List.fold_left
            (fun {venv; tenv; exps} d ->
               let {venv = venv'; tenv = tenv'; exps = exps'} = trans_dec venv tenv break level d in
                 {venv = venv'; tenv = tenv'; exps = exps @ exps'})
            {venv; tenv; exps = []}
            decs in
        let {ty; exp = body_exp} = trans_exp venv' tenv' break level body in
          {exp = Translate.let_exp assign_exps body_exp; ty}
    | A.ArrayExp (ty_id, size, init, pos) ->
        begin
          match S.look ty_id tenv with
          | Some ty ->
              begin
                match actual_ty ty with
                | T.ARRAY (elem_ty, _) as array_ty ->
                    let {exp = size_exp; _} as size_expty = trexp size in
                    let {exp = init_exp; _} as init_expty = trexp init in
                      checkint size_expty pos;
                      check_expty (actual_ty elem_ty) init_expty pos;
                      {exp = Translate.array_exp size_exp init_exp; ty = array_ty}
                | ty ->
                    Error_msg.error pos (Error_msg.Type_mismatch ("array", T.string_of_ty ty));
                    {exp = Translate.unit_exp; ty = T.ARRAY (T.INT, ref ())}
              end
          | None ->
              Error_msg.error pos (Error_msg.Undefined_array (S.name ty_id));
              {exp = Translate.unit_exp; ty = T.ARRAY (T.INT, ref ())}
        end

  in trexp exp

and trans_dec venv tenv break level = function
  | A.VarDec {A.vardec_name; vardec_ty; vardec_init; vardec_pos; vardec_escape} ->
      let access = Translate.alloc_local level !vardec_escape in
      let {exp; ty} as expty = trans_exp venv tenv break level vardec_init in
      let venv' =
        match vardec_ty with
        | None ->
            if ty = T.NIL then
              Error_msg.error vardec_pos Error_msg.Unconstrained_nil;
            S.enter vardec_name (Env.VarEntry (access, ty)) venv;
        | Some (ty_id, ty_pos) ->
            match S.look ty_id tenv with
            | Some ty ->
                let actual = actual_ty ty in
                  check_expty actual expty vardec_pos;
                  S.enter vardec_name (Env.VarEntry (access, actual)) venv
            | None ->
                Error_msg.error ty_pos (Error_msg.Undefined_type (S.name ty_id));
                S.enter vardec_name (Env.VarEntry (access, T.INT)) venv in
      let assign_exp = Translate.assign_exp (Translate.simple_var access level) exp in
        {venv = venv'; tenv; exps = [assign_exp]}

  | A.TypeDec tydecs ->
      let checkdups () =
        let rec loop seen = function
          | [] -> ()
          | {A.tydec_name; tydec_pos; _} :: xs ->
              if List.mem tydec_name seen
              then Error_msg.error tydec_pos (Error_msg.Duplicate_type_declaration (S.name tydec_name));
              loop (tydec_name :: seen) xs in
          loop [] tydecs in
      let nametys = List.map (fun {A.tydec_name; _} ->
          T.NAME (tydec_name, ref None))
          tydecs in
      let tenv' = List.fold_left
          (fun tenv namety -> match namety with
             | T.NAME (name, ty) as namety ->
                 S.enter name namety tenv
             | _ -> Error_msg.impossible "expected name type")
          tenv
          nametys in
      let actualtys = List.map (fun {A.tydec_ty; _ } ->
          trans_ty tenv' tydec_ty)
          tydecs in

        List.iter2 (fun namety actualty -> match namety with
            | T.NAME (name, ty) ->
                ty := Some actualty
            | _ -> Error_msg.impossible "expected name type")
          nametys
          actualtys;

        let checkcycles () =
          let edges = List.map (fun {A.tydec_name; tydec_ty; _} ->
              match tydec_ty with
              | A.NameTy (tyid, pos) ->
                  (match S.look tyid tenv' with
                   | Some (T.NAME (id, ty)) -> (tydec_name, Some id)
                   | _ -> (tydec_name, None))
              | _ -> (tydec_name, None))
              tydecs in
            List.iter
              (fun {A.tydec_name; tydec_pos; _} ->
                 let rec visit seen = function
                   | None -> ()
                   | Some v ->
                       if List.mem v seen
                       then Error_msg.error tydec_pos Error_msg.Illegal_cycle_in_type_declaration
                       else visit (v :: seen) (List.assoc v edges) in
                   visit [] (Some tydec_name))
              tydecs in

          checkdups ();
          checkcycles ();
          {venv; tenv = tenv'; exps = []}

  | A.FunctionDec fundecs ->
      let checkdups () =
        let rec loop seen = function
          | [] -> ()
          | {A.fundec_name; fundec_pos; _} :: xs ->
              if List.mem fundec_name seen then
                Error_msg.error fundec_pos (Error_msg.Duplicate_function_declaration (S.name fundec_name));
              loop (fundec_name :: seen) xs in
          loop [] fundecs in

      let header {A.fundec_name; fundec_params; fundec_result; _} =
        let trparam {A.ty=paramtyid; pos=paramtypos; _} =
          match S.look paramtyid tenv with
          | None ->
              Error_msg.error paramtypos (Error_msg.Undefined_type (S.name paramtyid));
              T.INT
          | Some paramty -> actual_ty paramty in
        let resultty =
          match fundec_result with
          | None -> T.UNIT
          | Some (resulttyid, resulttypos) ->
              match S.look resulttyid tenv with
              | None ->
                  Error_msg.error resulttypos (Error_msg.Undefined_type (S.name resulttyid));
                  T.INT
              | Some resultty ->
                  actual_ty resultty in
        let paramtys = List.map trparam fundec_params in
        let label = Temp.new_label () in
        let formal_escapes = List.map (fun {A.escape; _} -> !escape) fundec_params in
        let level' = Translate.new_level level label formal_escapes in
          (fundec_name, paramtys, resultty, label, level') in

      let headers = List.map header fundecs in

      let venv' =
        List.fold_left
          (fun venv (name, paramtys, resultty, label, level') ->
             S.enter name (Env.FunEntry (level', label, paramtys, resultty)) venv)
          venv
          headers in

        List.iter2
          (fun (name, param_tys, result_ty, _, level') {A.fundec_body; A.fundec_params; fundec_pos; _} ->
             let accesses = Translate.formals level' in
             let params = List.map2
                 (fun {A.name; _} ty -> (name, ty))
                 fundec_params
                 param_tys in
             let venv'' = List.fold_left2
                 (fun venv (name, ty) access -> S.enter name (Env.VarEntry (access, ty)) venv)
                 venv'
                 params
                 accesses in
             let {exp = body_exp; _} as body_expty = trans_exp venv'' tenv break level' fundec_body in
               check_expty result_ty body_expty fundec_pos;
               Translate.proc_entry_exit level' body_exp)
          headers
          fundecs;

        checkdups ();

        {venv=venv'; tenv; exps = []}

and trans_ty tenv = function
  | A.NameTy (ty, pos) ->
      begin
        match S.look ty tenv with
        | None ->
            Error_msg.error pos (Error_msg.Undefined_type (S.name ty));
            T.INT
        | Some ty' -> ty'
      end
  | A.RecordTy fields ->
      let trfield {A.name; escape; ty; pos} =
        let fieldty = match S.look ty tenv with
          | None -> Error_msg.error pos (Error_msg.Undefined_type (S.name ty));
              T.INT
          | Some ty -> ty in
          (name, fieldty) in
        T.RECORD (List.map trfield fields, ref ())
  | A.ArrayTy (ty, pos) ->
      let elem_ty = match S.look ty tenv with
        | None ->
            Error_msg.error pos (Error_msg.Undefined_type (S.name ty));
            T.INT
        | Some ty' -> ty' in
        T.ARRAY (elem_ty, ref ())

let trans_prog exp =
  let level = Translate.new_level Translate.outermost (Temp.new_label ()) [] in
  let {exp; _} = trans_exp Env.base_venv Env.base_tenv None level exp in
    Translate.proc_entry_exit level exp;
    let frags = Translate.get_result () in
      Translate.print_tree_canon exp;
      print_newline ();
      Translate.print_tree exp;
      print_newline ();
      Translate.print_frags frags;
      frags
