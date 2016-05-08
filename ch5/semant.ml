(*
   TODO:
   better error message with mismatched arity funcall
   prevent two types/functions with same name in same batch of decs (testcase 38, 39)
   break only appears witin a while or for loop
   constrain NIL to RECORD
   prevent reassignment to for loop variable
*)

module A = Absyn
module S = Symbol

type venv = Env.enventry S.table
type tenv = Types.ty S.table
type expty = {exp: Translate.exp; ty: Types.ty}
type decenv = {venv: venv; tenv: tenv}

let rec actual_ty = function
	| Types.NAME (s, ty) ->
		 (match !ty with
			| None -> raise (Failure (S.name s ^ " name type without actual type"))
			| Some actual -> actual_ty actual)
	| ty -> ty

let check_expty ty {exp; ty=ty'} pos =
	(match actual_ty ty, ty' with
	 | (Types.RECORD (_, uniq1)), (Types.RECORD (_, uniq2)) ->
			if not (uniq1 == uniq2)
			then Printf.eprintf "%d: different record types\n" pos
	 | (Types.ARRAY (_, uniq1)), (Types.ARRAY (_, uniq2)) ->
			if not (uniq1 == uniq2)
			then Printf.eprintf "%d: different array types\n" pos
	 | actual, ty' when actual <> ty' ->
			Printf.eprintf "%d: expected %s, got %s\n" pos (Types.string_of_ty actual) (Types.string_of_ty ty')
	 | _ -> ())

let check_int = check_expty Types.INT
let check_unit = check_expty Types.UNIT
												 
let rec trans_var venv tenv = function
  | A.SimpleVar (id, pos) ->
     (match S.look id venv with
      | Some Env.VarEntry ty ->
				 {exp=(); ty=ty}
      | _ ->
				 Printf.eprintf "%d: undefined variable %s\n" pos (S.name id);
				 {exp=(); ty=Types.INT}
     )
  | A.FieldVar (v, id, pos) ->
     let {ty; _} = trans_var venv tenv v in
     (match actual_ty ty with
      | Types.RECORD (fields, unique) ->
				 (try
						let (fieldname, fieldty) = List.find (fun (fieldname, fieldty) ->  fieldname = id) fields in
						{exp=(); ty=fieldty}
					with Not_found ->
						Printf.eprintf "%d: undefined field %s in record\n" pos (S.name id);
						{exp=();ty=Types.INT})
      | ty ->
				 Printf.eprintf "%d: expected record, got %s\n" pos (Types.string_of_ty ty);
				 {exp=(); ty=Types.INT})
  | A.SubscriptVar (v, exp, pos) ->
     let {ty; _} = trans_var venv tenv v in
     (match actual_ty ty with
      | Types.ARRAY (ty, unique) ->
				 check_int (trans_exp venv tenv exp) pos;
				 {exp=(); ty=ty}
      | _ ->
				 Printf.eprintf "%d: expected array, got %s\n" pos (Types.string_of_ty ty);
				 {exp=(); ty=Types.INT})

and trans_exp venv tenv exp =
  let rec trexp = function
    | A.VarExp v ->
			 let {exp; ty} = trans_var venv tenv v in
			 {exp; ty=actual_ty ty}
    | A.NilExp ->
       {exp=(); ty=Types.NIL}
    | A.IntExp i ->
       {exp=(); ty=Types.INT}
    | A.StringExp (s, pos) ->
       {exp=(); ty=Types.STRING}
    | A.CallExp (func, args, pos) ->
       (match S.look func venv with
				| Some Env.FunEntry (formaltys, result) ->
					 let check_arg ty expty = check_expty ty expty pos in
					 List.iter2 check_arg formaltys (List.map trexp args);
					 {exp=(); ty=result}
				| Some Env.VarEntry ty ->
					 Printf.eprintf "%d: expected function %s, got %s\n" pos (S.name func) (Types.string_of_ty ty);
					 {exp=(); ty=Types.INT}
				| None ->
					 Printf.eprintf "%d: undefined function %s\n" pos (S.name func);
					 {exp=(); ty=Types.INT})
		| A.OpExp (l, A.EqOp, r, pos) ->
			 let {ty=ty1; _} as expty1 = trexp l in
			 let {ty=ty2; _} as expty2 = trexp r in
			 (match ty1, ty2 with
				| Types.RECORD _, Types.RECORD _ -> ()
				| Types.ARRAY _, Types.ARRAY _-> ()
				| Types.INT, Types.INT -> ()
				| _ -> Printf.eprintf "%d: cannot compare %s with %s\n"
															pos (Types.string_of_ty ty1) (Types.string_of_ty ty2));
       {exp=(); ty=Types.INT}
		| A.OpExp (l, A.NeqOp, r, pos) ->
			 let {ty=ty1; _} as expty1 = trexp l in
			 let {ty=ty2; _} as expty2 = trexp r in
			 (match ty1, ty2 with
				| Types.RECORD _, Types.RECORD _ -> ()
				| Types.ARRAY _, Types.ARRAY _-> ()
				| Types.INT, Types.INT -> ()
				| _ -> Printf.eprintf "%d: cannot compare %s with %s\n"
															pos (Types.string_of_ty ty1) (Types.string_of_ty ty2));
       {exp=(); ty=Types.INT}
    | A.OpExp (l, op, r, pos) ->
       check_int (trexp l) pos;
       check_int (trexp r) pos;
       {exp=(); ty=Types.INT}
    | A.RecordExp (fields, tyid, pos) ->
			 (match S.look tyid tenv with
				| Some ty ->
					 (match actual_ty ty with
						| Types.RECORD (fieldtys, unique) as ty ->
							 List.iter2 (fun (s1,exp,pos) (s,ty) ->
									 if s1 <> s
									 then Printf.eprintf "%d: expected field %s, got %s\n" pos (S.name s) (S.name s1);
									 check_expty ty (trexp exp) pos) fields fieldtys;
							 {exp=(); ty=ty}
						| ty ->
							 Printf.eprintf "%d: expected record, got %s\n" pos (Types.string_of_ty ty);
							 {exp=(); ty=Types.RECORD([], ref ())})
				| None ->
					 Printf.eprintf "%d: undefined record %s\n" pos (S.name tyid);
					 {exp=(); ty=Types.RECORD([], ref ())})
    | A.SeqExp (exps) ->
       List.fold_left (fun _ (exp, pos) ->
					 trexp exp) {exp=(); ty=Types.UNIT} exps
    | A.AssignExp (v, e, pos) ->
       let {ty; _} = trans_var venv tenv v in
			 check_expty ty (trexp e) pos;
       {exp=(); ty=Types.UNIT}
    | A.IfExp (test, then', None, pos) ->
       check_int (trexp test) pos;
       check_unit (trexp then') pos;
			 {exp=(); ty=Types.UNIT}
    | A.IfExp (test, then', Some else', pos) ->
       check_int (trexp test) pos;
       let {ty=thenty; _} = trexp then' in
       let else_expty = trexp else' in
			 check_expty thenty else_expty pos;
       {exp=(); ty=thenty}
    | A.WhileExp (test, body, pos) ->
       check_int (trexp test) pos;
       let body_expty = trexp body in
			 check_expty Types.UNIT body_expty pos;
       {exp=(); ty=Types.UNIT}
    | A.ForExp (var, escape, lo, hi, body, pos) ->
       check_int (trexp lo) pos;
       check_int (trexp hi) pos;
       let venv' = S.enter var (Env.VarEntry Types.INT) venv in
			 let body_expty = trans_exp venv' tenv body in
			 check_expty Types.UNIT body_expty pos;
       {exp=(); ty=Types.UNIT}
    | A.BreakExp p ->
       {exp=(); ty=Types.UNIT}
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
						| Types.ARRAY (ty, unique) as aty ->
							 check_int (trexp size) pos;
							 let init_expty = trexp init in
							 check_expty ty init_expty pos;
							 {exp=(); ty=aty}
						| ty' ->
							 Printf.eprintf "%d: expected array, got %s\n" pos (Types.string_of_ty (actual_ty ty'));
							 {exp=(); ty=Types.ARRAY (Types.INT, ref ())})
				| None ->
					 Printf.eprintf "%d: undefined array %s\n" pos (S.name tyid);
					 {exp=(); ty=Types.ARRAY (Types.INT, ref ())})

  in trexp exp
					 
and trans_dec venv tenv = function
  | A.VarDec {A.vardec_name; vardec_ty=None; vardec_init; _} ->
     let {exp; ty} = trans_exp venv tenv vardec_init in
     {venv=S.enter vardec_name (Env.VarEntry ty) venv; tenv}
  | A.VarDec {A.vardec_name; vardec_ty=Some(tyname, typos); vardec_init; _} ->
     (match S.look tyname tenv with
			| Some ty ->
				 check_expty ty (trans_exp venv tenv vardec_init) typos;
				 {venv=S.enter vardec_name (Env.VarEntry ty) venv; tenv}
			| None ->
				 Printf.eprintf "%d: undefined type %s\n" typos (S.name tyname);
				 {venv=S.enter vardec_name (Env.VarEntry Types.INT) venv; tenv})
	| A.TypeDec tydecs ->
		 let nametys = List.map (fun {A.tydec_name; tydec_pos; _} ->
											 (Types.NAME (tydec_name, ref None), tydec_pos)) tydecs in
		 let enternamety tenv = function
			 | Types.NAME (tydec_name, _) as namety ->
					S.enter tydec_name namety tenv in
		 let tenv' = List.fold_left enternamety tenv (List.map fst nametys) in
		 let actualtys = List.map (fun {A.tydec_ty; _ } -> trans_ty tenv' tydec_ty) tydecs in
		 let setactualty namety actualty = match namety with
			 | Types.NAME (name, ty) -> ty := Some actualty in
		 let rec check_cycle pos seen = function
			 | Types.NAME (name, ty) ->
					if List.mem name seen
					then Printf.eprintf "%d: illegal cycle in mutually recursive type declarations\n" pos
					else (match !ty with
								| Some (Types.NAME (name2, _) as namety2) -> check_cycle pos (name :: seen) namety2
								| _ -> ()) in
		 List.iter2 setactualty (List.map fst nametys) actualtys;
		 List.iter (fun (namety, pos) -> check_cycle pos [] namety) nametys;
		 {venv; tenv=tenv'}
  | A.FunctionDec [{A.fundec_name; fundec_params; fundec_result; fundec_body; fundec_pos}] ->
		 let resultty = match fundec_result with
			 | None -> Types.UNIT
			 | Some (rty, rtypos) ->
					match S.look rty tenv with
					| Some ty -> actual_ty ty
					| None -> Printf.eprintf "%d: undefined type %s\n" rtypos (S.name rty);
										Types.INT in
     let trparam {A.name; escape; ty=pty; pos=ptypos} =
			 let paramty = match S.look pty tenv with
				 | Some ty -> actual_ty ty
				 | None -> Printf.eprintf "%d: undefined type %s\n" ptypos (S.name pty);
										Types.INT in
			 (name, paramty) in
     let params' = List.map trparam fundec_params in
     let venv' = S.enter fundec_name (Env.FunEntry ((List.map snd params'), resultty)) venv in
     let enterparam venv (name, ty) = S.enter name (Env.VarEntry ty) venv in
     let venv'' = List.fold_left enterparam venv' params' in
		 check_expty resultty (trans_exp venv'' tenv fundec_body) fundec_pos;
     {venv=venv'; tenv}
	| A.FunctionDec fundecs ->
		 let resultty = function
			 | None -> Types.UNIT
			 | Some (rty, rtypos) ->
					match S.look rty tenv with
					| Some ty -> actual_ty ty
					| None -> Printf.eprintf "%d: undefined type %s\n" rtypos (S.name rty);
										Types.INT in
     let trparam {A.ty=pty; pos=ptypos; _} =
			 match S.look pty tenv with
			 | Some ty -> actual_ty ty
			 | None -> Printf.eprintf "%d: undefined type %s\n" ptypos (S.name pty);
								 Types.INT in
		 let header {A.fundec_name; fundec_params; fundec_result; _} =
			 (fundec_name, List.map trparam fundec_params, resultty fundec_result) in
		 let enterheader venv (name, typarams, tyres) =
			 S.enter name (Env.FunEntry (typarams, tyres)) venv in
		 let venv' = List.fold_left enterheader venv (List.map header fundecs) in
		 ignore (List.map (fun fundec -> trans_dec venv' tenv (A.FunctionDec [fundec])) fundecs);
		 {venv=venv'; tenv}

and trans_ty tenv = function
  | A.NameTy (s, p) ->
		 (match S.look s tenv with
			| Some ty -> ty
			| None -> Printf.eprintf "%d: undefined type %s\n" p (S.name s);
								Types.INT)
  | A.RecordTy fields ->
     let trfield {A.name; escape; ty; pos} =
			 let fieldty = match S.look ty tenv with
				 | Some ty -> ty
				 | None -> Printf.eprintf "%d: undefined type %s\n" pos (S.name name);
									 Types.INT in
			 (name, fieldty) in
     Types.RECORD (List.map trfield fields, ref ())
  | A.ArrayTy (s, p) ->
		 let ty = match S.look s tenv with
			 | Some ty -> ty
			 | None -> Printf.eprintf "%d: undefined type %s\n" p (S.name s);
								 Types.INT in
     Types.ARRAY (ty, ref ())

let trans_prog exp =
  ignore (trans_exp Env.base_venv Env.base_tenv exp)
