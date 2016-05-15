(*
   TODO:
   prevent reassignment to for loop variable
*)

module A = Absyn
module S = Symbol
module T = Types

type venv = Env.enventry S.table
type tenv = T.t S.table
type expty = {exp: Translate.exp; ty: T.t}
type decenv = {venv: venv; tenv: tenv}

let check_break exp =
  let rec check inside = function
  | A.VarExp _ -> ()
  | A.NilExp -> ()
  | A.IntExp _ -> ()
  | A.StringExp _ -> ()
  | A.CallExp (func, args, pos) ->
     List.iter (fun arg -> check inside arg) args
  | A.OpExp (lexp, op, rexp, pos) ->
     check inside lexp;
     check inside rexp
  | A.RecordExp (fields, tyid, pos) ->
     List.iter (fun (field, exp, pos) -> check inside exp) fields
  | A.SeqExp exps ->
     List.iter (fun (exp, pos) -> check inside exp) exps
  | A.AssignExp (id, exp, pos) ->
     check inside exp
  | A.IfExp (test, then', None, pos) ->
     check inside test;
     check inside then'
  | A.IfExp (test, then', Some else', pos) ->
     check inside test;
     check inside then';
     check inside else'                 
  | A.WhileExp (test, body, pos) ->
     check true body
  | A.ForExp (var, escape, lo, hi, body, pos) ->
     check true body
  | A.BreakExp pos ->
     if not inside
     then Error_msg.error pos Error_msg.Illegal_break
  | A.LetExp (decs, body, p) ->
     List.iter (fun dec ->
         match dec with
         | A.FunctionDec fundecs ->
            List.iter (fun {A.fundec_body; _} -> check inside fundec_body) fundecs
         | A.VarDec {A.vardec_init; _} -> check inside vardec_init
         | A.TypeDec _ -> ()) decs;
     check inside body
  | A.ArrayExp (tyid, size, init, pos) ->
     check inside init in
  check false exp
                          
let rec actual_ty = function
  | T.NAME (s, ty) ->
     (match !ty with
      | None -> Error_msg.impossible "name type without actual type"
      | Some actual -> actual_ty actual)
  | ty -> ty

let check_expty ty {exp; ty=ty'} pos =
  (match actual_ty ty, ty' with
   | (T.RECORD (_, uniq1)), (T.RECORD (_, uniq2)) ->
      if uniq1 != uniq2
      then Error_msg.error pos (Error_msg.Record_type_mismatch)
   | T.RECORD _, T.NIL -> ()
   | T.NIL , T.RECORD _ -> ()
   | (T.ARRAY (_, uniq1)), (T.ARRAY (_, uniq2)) ->
      if uniq1 != uniq2
      then Error_msg.error pos (Error_msg.Array_type_mismatch)
   | actual, ty' when actual <> ty' ->
      Error_msg.error pos (Error_msg.Type_mismatch (T.string_of_ty actual, T.string_of_ty ty'))
   | _ -> ())

let check_int = check_expty T.INT
let check_unit = check_expty T.UNIT
			     
let rec trans_var venv tenv = function
  | A.SimpleVar (id, pos) ->
     (match S.look id venv with
      | Some Env.VarEntry ty ->
	 {exp=(); ty=ty}
      | _ ->
	 Error_msg.error pos (Error_msg.Undefined_variable (S.name id));
	 {exp=(); ty=T.INT}
     )
  | A.FieldVar (v, id, pos) ->
     let {ty; _} = trans_var venv tenv v in
     (match actual_ty ty with
      | T.RECORD (fields, unique) ->
	 (try
	    let (fieldname, fieldty) = List.find (fun (fieldname, fieldty) ->  fieldname = id) fields in
	    {exp=(); ty=fieldty}
	  with Not_found ->
	    Error_msg.error pos (Error_msg.Undefined_record_field (S.name id));
	    {exp=();ty=T.INT})
      | ty ->
	 Error_msg.error pos (Error_msg.Type_mismatch ("record", T.string_of_ty ty));
	 {exp=(); ty=T.INT})
  | A.SubscriptVar (v, exp, pos) ->
     let {ty; _} = trans_var venv tenv v in
     (match actual_ty ty with
      | T.ARRAY (ty, unique) ->
	 check_int (trans_exp venv tenv exp) pos;
	 {exp=(); ty=ty}
      | ty ->
	 Error_msg.error pos (Error_msg.Type_mismatch ("array", T.string_of_ty ty));
	 {exp=(); ty=T.INT})

and trans_exp venv tenv exp =
  let rec trexp = function
    | A.VarExp v ->
       let {exp; ty} = trans_var venv tenv v in
       {exp; ty=actual_ty ty}
    | A.NilExp ->
       {exp=(); ty=T.NIL}
    | A.IntExp i ->
       {exp=(); ty=T.INT}
    | A.StringExp (s, pos) ->
       {exp=(); ty=T.STRING}
    | A.CallExp (func, args, pos) ->
       (match S.look func venv with
	| Some Env.FunEntry (formaltys, result) ->
           let nformals = List.length formaltys in
           let nargs = List.length args in
           (if nformals <> nargs
	    then Error_msg.error pos (Error_msg.Arity_mismatch (nformals, nargs))
            else let check_arg ty expty = check_expty ty expty pos in
                 List.iter2 check_arg formaltys (List.map trexp args));
	   {exp=(); ty=result}
	| _ ->
	   Error_msg.error pos (Error_msg.Undefined_function (S.name func));
	   {exp=(); ty=T.INT})
    | A.OpExp (l, A.EqOp, r, pos) ->
       let {ty=ty1; _} as expty1 = trexp l in
       let {ty=ty2; _} as expty2 = trexp r in
       (match ty1, ty2 with
	| T.RECORD _, T.RECORD _ -> ()
        | T.RECORD _, T.NIL -> ()
        | T.NIL, T.RECORD _ -> ()
	| T.ARRAY _, T.ARRAY _ -> ()
	| T.INT, T.INT -> ()
	| _ -> Error_msg.error pos (Error_msg.Illegal_comparison (T.string_of_ty ty1, T.string_of_ty ty2)));
       {exp=(); ty=T.INT}
    | A.OpExp (l, A.NeqOp, r, pos) ->
       let {ty=ty1; _} as expty1 = trexp l in
       let {ty=ty2; _} as expty2 = trexp r in
       (match ty1, ty2 with
	| T.RECORD _, T.RECORD _ -> ()
        | T.RECORD _, T.NIL -> ()
        | T.NIL, T.RECORD _ -> ()
	| T.ARRAY _, T.ARRAY _-> ()
	| T.INT, T.INT -> ()
	| _ -> Error_msg.error pos (Error_msg.Illegal_comparison (T.string_of_ty ty1, T.string_of_ty ty2)));
       {exp=(); ty=T.INT}
    | A.OpExp (l, op, r, pos) ->
       check_int (trexp l) pos;
       check_int (trexp r) pos;
       {exp=(); ty=T.INT}
    | A.RecordExp (fields, tyid, pos) ->
       (match S.look tyid tenv with
	| Some ty ->
	   (match actual_ty ty with
	    | T.RECORD (fieldtys, unique) as ty ->
	       List.iter2 (fun (s1,exp,pos) (s,ty) ->
		   if s1 <> s
		   then Error_msg.error pos (Error_msg.Record_field_mismatch (S.name s, S.name s1));
		   check_expty ty (trexp exp) pos) fields fieldtys;
	       {exp=(); ty=ty}
	    | ty ->
	       Error_msg.error pos (Error_msg.Type_mismatch ("record", T.string_of_ty ty));
	       {exp=(); ty=T.RECORD([], ref ())})
	| None ->
	   Error_msg.error pos (Error_msg.Undefined_record (S.name tyid));
	   {exp=(); ty=T.RECORD([], ref ())})
    | A.SeqExp (exps) ->
       List.fold_left (fun _ (exp, pos) ->
	   trexp exp) {exp=(); ty=T.UNIT} exps
    | A.AssignExp (v, e, pos) ->
       let {ty; _} = trans_var venv tenv v in
       check_expty ty (trexp e) pos;
       {exp=(); ty=T.UNIT}
    | A.IfExp (test, then', None, pos) ->
       check_int (trexp test) pos;
       check_unit (trexp then') pos;
       {exp=(); ty=T.UNIT}
    | A.IfExp (test, then', Some else', pos) ->
       check_int (trexp test) pos;
       let {ty=thenty; _} = trexp then' in
       let else_expty = trexp else' in
       check_expty thenty else_expty pos;
       {exp=(); ty=thenty}
    | A.WhileExp (test, body, pos) ->
       check_int (trexp test) pos;
       let body_expty = trexp body in
       check_expty T.UNIT body_expty pos;
       {exp=(); ty=T.UNIT}
    | A.ForExp (var, escape, lo, hi, body, pos) ->
       check_int (trexp lo) pos;
       check_int (trexp hi) pos;
       let venv' = S.enter var (Env.VarEntry T.INT) venv in
       let body_expty = trans_exp venv' tenv body in
       check_expty T.UNIT body_expty pos;
       {exp=(); ty=T.UNIT}
    | A.BreakExp p ->
       {exp=(); ty=T.UNIT}
    | A.LetExp (decs, body, p) ->
       let {venv=venv'; tenv=tenv'} =
	 List.fold_left
	   (fun {venv; tenv} dec -> trans_dec venv tenv dec)
	   {venv; tenv} decs in
       trans_exp venv' tenv' body
    | A.ArrayExp (tyid, size, init, pos) ->
       (match S.look tyid tenv with
	| Some ty ->
	   (match actual_ty ty with
	    | T.ARRAY (ty, unique) as aty ->
	       check_int (trexp size) pos;
	       let init_expty = trexp init in
	       check_expty ty init_expty pos;
	       {exp=(); ty=aty}
	    | ty' ->
	       Error_msg.error pos (Error_msg.Type_mismatch ("array", T.string_of_ty ty'));
	       {exp=(); ty=T.ARRAY (T.INT, ref ())})
	| None ->
	   Error_msg.error pos (Error_msg.Undefined_array (S.name tyid));
	   {exp=(); ty=T.ARRAY (T.INT, ref ())})

  in trexp exp
	   
and trans_dec venv tenv = function
  | A.VarDec {A.vardec_name; vardec_ty=None; vardec_init; vardec_pos} ->
     let {exp; ty} = trans_exp venv tenv vardec_init in
     if ty=T.NIL
     then Error_msg.error vardec_pos Error_msg.Unconstrained_nil;
     {venv=S.enter vardec_name (Env.VarEntry ty) venv; tenv}
  | A.VarDec {A.vardec_name; vardec_ty=Some(tyname, typos); vardec_init; _} ->
     (match S.look tyname tenv with
      | Some ty ->
	 check_expty ty (trans_exp venv tenv vardec_init) typos;
	 {venv=S.enter vardec_name (Env.VarEntry ty) venv; tenv}
      | None ->
	 Error_msg.error typos (Error_msg.Undefined_type (S.name tyname));
	 {venv=S.enter vardec_name (Env.VarEntry T.INT) venv; tenv})
  | A.TypeDec tydecs ->
     let nametys = List.fold_right (fun {A.tydec_name; tydec_pos; _} tys ->
                       if List.exists (fun (name, pos) ->
                              match name with
                              | T.NAME (tydec_name2, _) -> tydec_name=tydec_name2) tys
		       then Error_msg.error tydec_pos (Error_msg.Duplicate_type_declaration (S.name tydec_name));
		       (T.NAME (tydec_name, ref None), tydec_pos) :: tys) tydecs [] in
     let enternamety tenv = function
       | T.NAME (tydec_name, _) as namety ->
	  S.enter tydec_name namety tenv in
     let tenv' = List.fold_left enternamety tenv (List.map fst nametys) in
     let actualtys = List.map (fun {A.tydec_ty; _ } -> trans_ty tenv' tydec_ty) tydecs in
     let setactualty namety actualty = match namety with
       | T.NAME (name, ty) -> ty := Some actualty in
     let rec check_cycle pos seen = function
       | T.NAME (name, ty) ->
	  if List.mem name seen
	  then Error_msg.error pos Error_msg.Illegal_cycle_in_type_declaration
	  else (match !ty with
		| Some (T.NAME (name2, _) as namety2) -> check_cycle pos (name :: seen) namety2
		| _ -> ()) in
     List.iter2 setactualty (List.map fst nametys) actualtys;
     List.iter (fun (namety, pos) -> check_cycle pos [] namety) nametys;
     {venv; tenv=tenv'}
  | A.FunctionDec [{A.fundec_name; fundec_params; fundec_result; fundec_body; fundec_pos}] ->
     let resultty = match fundec_result with
       | None -> T.UNIT
       | Some (rty, rtypos) ->
	  match S.look rty tenv with
	  | Some ty -> actual_ty ty
	  | None -> Error_msg.error rtypos (Error_msg.Undefined_type (S.name rty));
		    T.INT in
     let trparam {A.name; escape; ty=pty; pos=ptypos} =
       let paramty = match S.look pty tenv with
	 | Some ty -> actual_ty ty
	 | None -> Error_msg.error ptypos (Error_msg.Undefined_type (S.name pty));
		   T.INT in
       (name, paramty) in
     let params' = List.map trparam fundec_params in
     let venv' = S.enter fundec_name (Env.FunEntry ((List.map snd params'), resultty)) venv in
     let enterparam venv (name, ty) = S.enter name (Env.VarEntry ty) venv in
     let venv'' = List.fold_left enterparam venv' params' in
     check_expty resultty (trans_exp venv'' tenv fundec_body) fundec_pos;
     {venv=venv'; tenv}
  | A.FunctionDec fundecs ->
     let resultty = function
       | None -> T.UNIT
       | Some (rty, rtypos) ->
	  match S.look rty tenv with
	  | Some ty -> actual_ty ty
	  | None -> Error_msg.error rtypos (Error_msg.Undefined_type (S.name rty));
		    T.INT in
     let trparam {A.ty=pty; pos=ptypos; _} =
       match S.look pty tenv with
       | Some ty -> actual_ty ty
       | None -> Error_msg.error ptypos (Error_msg.Undefined_type (S.name pty));
		 T.INT in
     let header {A.fundec_name; fundec_params; fundec_result; _} =
       (fundec_name, List.map trparam fundec_params, resultty fundec_result) in
     let enterheader venv (name, typarams, tyres) =
       S.enter name (Env.FunEntry (typarams, tyres)) venv in
     let headers = List.fold_right (fun ({A.fundec_name; fundec_pos; _ } as fundec) hs ->
                       if List.exists (fun (fundec_name2, _, _) -> fundec_name2=fundec_name) hs
		       then Error_msg.error fundec_pos (Error_msg.Duplicate_function_declaration (S.name fundec_name));
                       header fundec :: hs) fundecs [] in
     let venv' = List.fold_left enterheader venv headers in
     ignore (List.map (fun fundec -> trans_dec venv' tenv (A.FunctionDec [fundec])) fundecs);
     {venv=venv'; tenv}

and trans_ty tenv = function
  | A.NameTy (name, pos) ->
     (match S.look name tenv with
      | Some ty -> ty
      | None -> Error_msg.error pos (Error_msg.Undefined_type (S.name name));
		T.INT)
  | A.RecordTy fields ->
     let trfield {A.name; escape; ty; pos} =
       let fieldty = match S.look ty tenv with
	 | Some ty -> ty
	 | None -> Error_msg.error pos (Error_msg.Undefined_type (S.name name));
		   T.INT in
       (name, fieldty) in
     T.RECORD (List.map trfield fields, ref ())
  | A.ArrayTy (name, pos) ->
     let ty = match S.look name tenv with
       | Some ty -> ty
       | None -> Error_msg.error pos (Error_msg.Undefined_type (S.name name));
		 T.INT in
     T.ARRAY (ty, ref ())

let trans_prog exp =
  ignore (trans_exp Env.base_venv Env.base_tenv exp);
  ignore (check_break exp);
