(*
   TODO:
   check breaks in trans_exp
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

let checktydups tydecs =
  let rec loop seen = function
    | [] -> ()
    | ({A.tydec_name; tydec_pos; _} as tydec) :: xs ->
       if List.mem tydec_name seen
       then Error_msg.error tydec_pos (Error_msg.Duplicate_type_declaration (S.name tydec_name));
       loop (tydec_name :: seen) xs in
  loop [] tydecs

let checkfundups fundecs =
  let rec loop seen = function
    | [] -> ()
    | ({A.fundec_name; fundec_pos; _} as fundec) :: xs ->
       if List.mem fundec_name seen
       then Error_msg.error fundec_pos (Error_msg.Duplicate_function_declaration (S.name fundec_name));
       loop (fundec_name :: seen) xs in
  loop [] fundecs
       
let checkcomp lty rty pos =
    match lty, rty  with
    | T.RECORD _, T.RECORD _ -> ()
    | T.RECORD _, T.NIL -> ()
    | T.NIL, T.RECORD _ -> ()
    | T.ARRAY _, T.ARRAY _ -> ()
    | T.INT, T.INT -> ()
    | _ -> Error_msg.error pos (Error_msg.Illegal_comparison (T.string_of_ty lty, T.string_of_ty rty))
			   
       (*
let rec check_cycle pos seen = function
  | T.NAME (name, ty) ->
     if List.mem name seen
     then Error_msg.error pos Error_msg.Illegal_cycle_in_type_declaration
     else (match !ty with
	   | Some (T.NAME (name2, _) as namety2) -> check_cycle pos (name :: seen) namety2
	   | _ -> ()) in
	*)

let rec trans_var venv tenv = function
  | A.SimpleVar (id, pos) ->
     (match S.look id venv with
      | Some Env.VarEntry ty ->
	 {exp=(); ty}
      | _ ->
	 Error_msg.error pos (Error_msg.Undefined_variable (S.name id));
	 {exp=(); ty=T.INT}
     )
  | A.FieldVar (var, id, pos) ->
     let {ty; _} = trans_var venv tenv var in
     (match actual_ty ty with
      | T.RECORD (fields, unique) ->
	 (try
	    let (fieldname, fieldty) = List.find
					 (fun (fieldname, fieldty) -> fieldname=id)
					 fields in
	    {exp=(); ty=fieldty}
	  with Not_found ->
	    Error_msg.error pos (Error_msg.Undefined_record_field (S.name id));
	    {exp=();ty=T.INT})
      | ty ->
	 Error_msg.error pos (Error_msg.Type_mismatch ("record", T.string_of_ty ty));
	 {exp=(); ty=T.INT})
  | A.SubscriptVar (var, exp, pos) ->
     let {ty; _} = trans_var venv tenv var in
     (match actual_ty ty with
      | T.ARRAY (elemty, unique) ->
	 check_int (trans_exp venv tenv exp) pos;
	 {exp=(); ty=elemty}
      | otherty ->
	 Error_msg.error pos (Error_msg.Type_mismatch ("array", T.string_of_ty otherty));
	 {exp=(); ty=T.INT})

and trans_exp venv tenv exp =

  let rec trexp = function
    | A.VarExp var ->
       let {exp; ty} = trans_var venv tenv var in
       {exp; ty=actual_ty ty}
    | A.NilExp ->
       {exp=(); ty=T.NIL}
    | A.IntExp i ->
       {exp=(); ty=T.INT}
    | A.StringExp (str, pos) ->
       {exp=(); ty=T.STRING}
    | A.CallExp (funcname, args, pos) ->
       (match S.look funcname venv with
	| Some Env.FunEntry (formaltys, resultty) ->
           let numformals = List.length formaltys in
           let numactuals = List.length args in
           if numformals <> numactuals
	   then Error_msg.error pos (Error_msg.Arity_mismatch (numformals, numactuals))
           else List.iter2 (fun formalty arg -> check_expty formalty (trexp arg) pos)
			   formaltys args;
	   {exp=(); ty=resultty}
	| _ ->
	   Error_msg.error pos (Error_msg.Undefined_function (S.name funcname));
	   {exp=(); ty=T.INT})
    | A.OpExp (l, A.EqOp, r, pos) ->
       let {ty=lty; _} as expty1 = trexp l in
       let {ty=rty; _} as expty2 = trexp r in
       checkcomp lty rty pos;
       {exp=(); ty=T.INT}    
    | A.OpExp (l, A.NeqOp, r, pos) ->
       let {ty=lty; _} as expty1 = trexp l in
       let {ty=rty; _} as expty2 = trexp r in
       checkcomp lty rty pos;
       {exp=(); ty=T.INT}
    | A.OpExp (l, op, r, pos) ->
       check_int (trexp l) pos;
       check_int (trexp r) pos;
       {exp=(); ty=T.INT}
    | A.RecordExp (fields, recordtyid, pos) ->
       (match S.look recordtyid tenv with
	| Some ty ->
	   (match actual_ty ty with
	    | T.RECORD (fieldtys, unique) as recordty ->
	       List.iter2 (fun (fieldname1, ty) (fieldname2, exp, pos) ->
		   if fieldname1 <> fieldname2 
		   then Error_msg.error pos (Error_msg.Record_field_mismatch (S.name fieldname1, S.name fieldname2));
		   check_expty ty (trexp exp) pos)
			  fieldtys
			  fields;
	       {exp=(); ty=recordty}
	    | otherty ->
	       Error_msg.error pos (Error_msg.Type_mismatch ("record", T.string_of_ty otherty));
	       {exp=(); ty=T.RECORD([], ref ())})
	| None ->
	   Error_msg.error pos (Error_msg.Undefined_record (S.name recordtyid));
	   {exp=(); ty=T.RECORD([], ref ())})
    | A.SeqExp (exps) ->
       List.fold_left (fun _ (exp, pos) -> trexp exp)
		      {exp=(); ty=T.UNIT}
		      exps
    | A.AssignExp (var, exp, pos) ->
       let {ty; _} = trans_var venv tenv var in
       check_expty ty (trexp exp) pos;
       {exp=(); ty=T.UNIT}
    | A.IfExp (test, then', None, pos) ->
       check_int (trexp test) pos;
       check_unit (trexp then') pos;
       {exp=(); ty=T.UNIT}
    | A.IfExp (test, then', Some else', pos) ->
       check_int (trexp test) pos;
       let thenexpty = trexp then' in
       let elseexpty = trexp else' in
       check_expty thenexpty.ty elseexpty pos;
       {exp=(); ty=thenexpty.ty}
    | A.WhileExp (test, body, pos) ->
       check_int (trexp test) pos;
       let bodyexpty = trexp body in
       check_unit bodyexpty pos;
       {exp=(); ty=T.UNIT}
    | A.ForExp (var, escape, loexp, hiexp, body, pos) ->
       check_int (trexp loexp) pos;
       check_int (trexp hiexp) pos;
       let venv' = S.enter var (Env.VarEntry T.INT) venv in
       let bodyexpty = trans_exp venv' tenv body in
       check_unit bodyexpty pos;
       {exp=(); ty=T.UNIT}
    | A.BreakExp pos ->
       {exp=(); ty=T.UNIT}
    | A.LetExp (decs, body, pos) ->
       let {venv=venv'; tenv=tenv'} = List.fold_left
					(fun {venv; tenv} dec -> trans_dec venv tenv dec)
					{venv; tenv}
					decs in
       trans_exp venv' tenv' body
    | A.ArrayExp (arraytyid, sizeexp, initexp, pos) ->
       (match S.look arraytyid tenv with
	| Some ty ->
	   (match actual_ty ty with
	    | T.ARRAY (elemty, unique) as arrayty ->
	       check_int (trexp sizeexp) pos;
	       check_expty elemty (trexp initexp) pos;
	       {exp=(); ty=arrayty}
	    | otherty ->
	       Error_msg.error pos (Error_msg.Type_mismatch ("array", T.string_of_ty otherty));
	       {exp=(); ty=T.ARRAY (T.INT, ref ())})
	| None ->
	   Error_msg.error pos (Error_msg.Undefined_array (S.name arraytyid));
	   {exp=(); ty=T.ARRAY (T.INT, ref ())})

  in trexp exp
	   
and trans_dec venv tenv = function
  | A.VarDec {A.vardec_name; vardec_ty; vardec_init; vardec_pos} ->
     let venv' = match vardec_ty with
       | None ->
	  let {ty; _} = trans_exp venv tenv vardec_init in
	  if ty = T.NIL then Error_msg.error vardec_pos Error_msg.Unconstrained_nil;
	  S.enter vardec_name (Env.VarEntry ty) venv;
       | Some (tyid, typos) ->
	  match S.look tyid tenv with
	  | Some ty ->
	     check_expty ty (trans_exp venv tenv vardec_init) vardec_pos;
	     S.enter vardec_name (Env.VarEntry ty) venv
	  | None ->
	     Error_msg.error typos (Error_msg.Undefined_type (S.name tyid));
	     S.enter vardec_name (Env.VarEntry T.INT) venv in
     {venv=venv'; tenv}
  | A.TypeDec tydecs ->
     let nametys = List.map (fun {A.tydec_name; _} ->
		       T.NAME (tydec_name, ref None))
			    tydecs in
     let tenv' = List.fold_left (fun tenv (T.NAME (name, ty) as namety) ->
		     S.enter name namety tenv)
				tenv
				nametys in
     let actualtys = List.map (fun {A.tydec_ty; _ } ->
			 trans_ty tenv' tydec_ty)
		       tydecs in
     List.iter2 (fun (T.NAME (name, ty)) actualty -> ty := Some actualty)
		nametys
		actualtys;
     checktydups tydecs;
     (* checkcycle venv'; *)
     {venv; tenv=tenv'}
  | A.FunctionDec fundecs ->
     let header {A.fundec_name; fundec_params; fundec_result; _} =
       let trparam {A.ty=paramtyid; pos=paramtypos; _} =
	 match S.look paramtyid tenv with
	 | None -> Error_msg.error paramtypos (Error_msg.Undefined_type (S.name paramtyid));
		   T.INT
	 | Some paramty -> paramty in
       let resultty = match fundec_result with
	 | None -> T.UNIT
	 | Some (resulttyid, resulttypos) ->
	    match S.look resulttyid tenv with
	    | None -> Error_msg.error resulttypos (Error_msg.Undefined_type (S.name resulttyid));
		      T.INT
	    | Some resultty -> resultty in
       (fundec_name, List.map trparam fundec_params, resultty) in
     let headers = List.map header fundecs in
     let venv' = List.fold_left (fun venv (name, paramtys, resultty) ->
		     S.enter name (Env.FunEntry (paramtys, resultty)) venv)
				venv
				headers in
     List.iter2 (fun (name, paramtys, resultty) {A.fundec_body; A.fundec_params; fundec_pos; _} ->
	 let venv'' = List.fold_left2 (fun venv {A.name=paramname; _} paramty ->
			  S.enter paramname (Env.VarEntry paramty) venv)
				      venv'
				      fundec_params
				      paramtys in
	 let bodyexpty = trans_exp venv'' tenv fundec_body in
	 check_expty resultty bodyexpty fundec_pos)
		headers
		fundecs;
     checkfundups fundecs;
     {venv=venv'; tenv}
       
and trans_ty tenv = function
  | A.NameTy (ty, pos) ->
     (match S.look ty tenv with
      | None -> Error_msg.error pos (Error_msg.Undefined_type (S.name ty));
		T.INT
      | Some ty -> ty)
  | A.RecordTy fields ->
     let trfield {A.name; escape; ty; pos} =
       let fieldty = match S.look ty tenv with
	 | None -> Error_msg.error pos (Error_msg.Undefined_type (S.name ty));
		   T.INT
	 | Some ty -> ty in
       (name, fieldty) in
     T.RECORD (List.map trfield fields, ref ())
  | A.ArrayTy (ty, pos) ->
     let elemty = match S.look ty tenv with
       | None -> Error_msg.error pos (Error_msg.Undefined_type (S.name ty));
		 T.INT
       | Some otherty -> otherty in
     T.ARRAY (elemty, ref ())

let trans_prog exp =
  ignore (trans_exp Env.base_venv Env.base_tenv exp);
  ignore (check_break exp);
