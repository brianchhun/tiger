module A = Absyn
module S = Symbol
module T = Types

type venv = Env.enventry S.table
type tenv = T.t S.table
type expty = {exp: Translate.exp; ty: T.t}
type decenv = {venv: venv; tenv: tenv}
	    
let check_expty ty1 {exp; ty=ty2} pos =
  match ty1, ty2 with
  | T.RECORD (_, unique1), T.RECORD (_, unique2) ->
     if unique1 != unique2
     then Error_msg.error pos (Error_msg.Record_type_mismatch)
  | T.RECORD _, T.NIL -> ()
  | T.NIL , T.RECORD _ -> ()
  | T.ARRAY (_, unique1), T.ARRAY (_, unique2) ->
     if unique1 != unique2
     then Error_msg.error pos (Error_msg.Array_type_mismatch)
  | _ ->
     if ty1 <> ty2
     then Error_msg.error pos (Error_msg.Type_mismatch (T.string_of_ty ty1, T.string_of_ty ty2))
	   
let checkint = check_expty T.INT
let checkunit = check_expty T.UNIT
			    
let rec actual_ty = function
  | T.NAME (id, ty) ->
     (match !ty with
      | None -> Error_msg.impossible "name type without actual type"
      | Some actual -> actual_ty actual)
  | ty -> ty
	    
let rec trans_var venv tenv breakable = function
  | A.SimpleVar (id, pos) ->
     (match S.look id venv with
      | Some Env.VarEntry ty ->
	 {exp=(); ty}
      | _ ->
	 Error_msg.error pos (Error_msg.Undefined_variable (S.name id));
	 {exp=(); ty=T.INT}
     )
  | A.FieldVar (var, id, pos) ->
     let {ty; _} = trans_var venv tenv breakable var in
     (match ty with
      | T.RECORD (fields, unique) ->
	 (try
	    let (fieldname, fieldty) = List.find
					 (fun (fieldname, fieldty) -> fieldname=id)
					 fields in
	    {exp=(); ty=actual_ty fieldty}
	  with Not_found ->
	    Error_msg.error pos (Error_msg.Undefined_record_field (S.name id));
	    {exp=();ty=T.INT})
      | ty ->
	 Error_msg.error pos (Error_msg.Type_mismatch ("record", T.string_of_ty ty));
	 {exp=(); ty=T.INT})
  | A.SubscriptVar (var, exp, pos) ->
     let {ty; _} = trans_var venv tenv breakable var in
     (match ty with
      | T.ARRAY (elemty, unique) ->
	 checkint (trans_exp venv tenv breakable exp) pos;
	 {exp=(); ty=actual_ty elemty}
      | otherty ->
	 Error_msg.error pos (Error_msg.Type_mismatch ("array", T.string_of_ty otherty));
	 {exp=(); ty=T.INT})

and trans_exp venv tenv breakable exp =

  let checkcomp lty rty pos =
    match lty, rty  with
    | T.RECORD _, T.RECORD _ -> ()
    | T.RECORD _, T.NIL      -> ()
    | T.NIL     , T.RECORD _ -> ()
    | T.ARRAY _ , T.ARRAY _  -> ()
    | T.INT     , T.INT      -> ()
    | _ -> Error_msg.error pos (Error_msg.Illegal_comparison (T.string_of_ty lty, T.string_of_ty rty)) in

  let rec trexp = function
    | A.VarExp var ->
       let {exp; ty} = trans_var venv tenv breakable var in
       {exp; ty}
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
			   formaltys
			   args;
	   {exp=(); ty=resultty}
	| _ ->
	   Error_msg.error pos (Error_msg.Undefined_function (S.name funcname));
	   {exp=(); ty=T.INT})
    | A.OpExp (l, A.EqOp, r, pos) ->
       let {ty=lty; _} = trexp l in
       let {ty=rty; _} = trexp r in
       checkcomp lty rty pos;
       {exp=(); ty=T.INT}
    | A.OpExp (l, A.NeqOp, r, pos) ->
       let {ty=lty; _} = trexp l in
       let {ty=rty; _} = trexp r in
       checkcomp lty rty pos;
       {exp=(); ty=T.INT}
    | A.OpExp (l, op, r, pos) ->
       checkint (trexp l) pos;
       checkint (trexp r) pos;
       {exp=(); ty=T.INT}
    | A.RecordExp (fields, recordtyid, pos) ->
       (match S.look recordtyid tenv with
	| Some ty ->
	   (match actual_ty ty with
	    | T.RECORD (fieldtys, unique) as recordty ->
	       List.iter2 (fun (fieldname1, ty) (fieldname2, exp, pos) ->
		   if fieldname1 <> fieldname2
		   then Error_msg.error pos (Error_msg.Record_field_mismatch (S.name fieldname1, S.name fieldname2));
		   check_expty (actual_ty ty) (trexp exp) pos)
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
       let {ty; _} = trans_var venv tenv breakable var in
       check_expty ty (trexp exp) pos;
       {exp=(); ty=T.UNIT}
    | A.IfExp (test, then', None, pos) ->
       checkint (trexp test) pos;
       checkunit (trexp then') pos;
       {exp=(); ty=T.UNIT}
    | A.IfExp (test, then', Some else', pos) ->
       checkint (trexp test) pos;
       let thenexpty = trexp then' in
       let elseexpty = trexp else' in
       check_expty thenexpty.ty elseexpty pos;
       {exp=(); ty=thenexpty.ty}
    | A.WhileExp (test, body, pos) ->
       checkint (trexp test) pos;
       let bodyexpty = trans_exp venv tenv true body in
       checkunit bodyexpty pos;
       {exp=(); ty=T.UNIT}
    | A.ForExp (var, escape, loexp, hiexp, body, pos) ->
       checkint (trexp loexp) pos;
       checkint (trexp hiexp) pos;
       let venv' = S.enter var (Env.VarEntry T.INT) venv in
       let bodyexpty = trans_exp venv' tenv true body in
       checkunit bodyexpty pos;
       {exp=(); ty=T.UNIT}
    | A.BreakExp pos ->
       if not breakable
       then Error_msg.error pos Error_msg.Illegal_break;
       {exp=(); ty=T.UNIT}
    | A.LetExp (decs, body, pos) ->
       let {venv=venv'; tenv=tenv'} = List.fold_left
					(fun {venv; tenv} dec -> trans_dec venv tenv breakable dec)
					{venv; tenv}
					decs in
       trans_exp venv' tenv' breakable body
    | A.ArrayExp (arraytyid, sizeexp, initexp, pos) ->
       (match S.look arraytyid tenv with
	| Some ty ->
	   (match actual_ty ty with
	    | T.ARRAY (elemty, unique) as arrayty ->
	       checkint (trexp sizeexp) pos;
	       check_expty (actual_ty elemty) (trexp initexp) pos;
	       {exp=(); ty=arrayty}
	    | otherty ->
	       Error_msg.error pos (Error_msg.Type_mismatch ("array", T.string_of_ty otherty));
	       {exp=(); ty=T.ARRAY (T.INT, ref ())})
	| None ->
	   Error_msg.error pos (Error_msg.Undefined_array (S.name arraytyid));
	   {exp=(); ty=T.ARRAY (T.INT, ref ())})

  in trexp exp

and trans_dec venv tenv breakable = function
  | A.VarDec {A.vardec_name; vardec_ty; vardec_init; vardec_pos} ->
     let venv' = match vardec_ty with
       | None ->
	  let {ty; _} = trans_exp venv tenv breakable vardec_init in
	  if ty = T.NIL then Error_msg.error vardec_pos Error_msg.Unconstrained_nil;
	  S.enter vardec_name (Env.VarEntry ty) venv;
       | Some (tyid, typos) ->
	  match S.look tyid tenv with
	  | Some ty ->
	     let actualty = actual_ty ty in
	     check_expty actualty (trans_exp venv tenv breakable vardec_init) vardec_pos;
	     S.enter vardec_name (Env.VarEntry actualty) venv
	  | None ->
	     Error_msg.error typos (Error_msg.Undefined_type (S.name tyid));
	     S.enter vardec_name (Env.VarEntry T.INT) venv in
     {venv=venv'; tenv}
  | A.TypeDec tydecs ->
     let checkdups() =
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
     let checkcycles() =
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
     checkdups();
     checkcycles();
     {venv; tenv=tenv'}
  | A.FunctionDec fundecs ->
     let checkdups() =
       let rec loop seen = function
	 | [] -> ()
	 | {A.fundec_name; fundec_pos; _} :: xs ->
	    if List.mem fundec_name seen
	    then Error_msg.error fundec_pos (Error_msg.Duplicate_function_declaration (S.name fundec_name));
	    loop (fundec_name :: seen) xs in
       loop [] fundecs in
     let header {A.fundec_name; fundec_params; fundec_result; _} =
       let trparam {A.ty=paramtyid; pos=paramtypos; _} =
	 match S.look paramtyid tenv with
	 | None -> Error_msg.error paramtypos (Error_msg.Undefined_type (S.name paramtyid));
		   T.INT
	 | Some paramty -> actual_ty paramty in
       let resultty = match fundec_result with
	 | None -> T.UNIT
	 | Some (resulttyid, resulttypos) ->
	    match S.look resulttyid tenv with
	    | None -> Error_msg.error resulttypos (Error_msg.Undefined_type (S.name resulttyid));
		      T.INT
	    | Some resultty -> actual_ty resultty in
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
	 let bodyexpty = trans_exp venv'' tenv breakable fundec_body in
	 check_expty resultty bodyexpty fundec_pos)
		headers
		fundecs;
     checkdups();
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
  ignore (trans_exp Env.base_venv Env.base_tenv false exp)
