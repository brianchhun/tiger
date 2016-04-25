module A = Absyn
module S = Symbol

type venv = Env.enventry S.table
type tenv = Types.ty S.table
type expty = {exp: Translate.exp; ty: Types.ty}
type decenv = {venv: venv; tenv: tenv}

let error s pos =
  prerr_endline ((string_of_int pos) ^ ":" ^ s)

let check_int {exp; ty} pos =
  if ty <> Types.INT then
    error ("expected int " ^ string_of_int pos) pos;
  {exp; ty=Types.INT}

let check_unit {exp; ty} pos =
  if ty <> Types.UNIT then
    error ("expected unit at " ^ string_of_int pos) pos;
  {exp; ty=Types.UNIT}

let rec trans_var venv tenv = function
  | A.SimpleVar (id, pos) ->
     (match S.look id venv with
      | Env.VarEntry ty ->
	  {exp=(); ty=ty}
      | _ ->
	 error ("undefined variable " ^ S.name id) pos;
	 {exp=(); ty=Types.INT}
     )
  | A.FieldVar (v, id, pos) ->
     let {ty; _} = trans_var venv tenv v in
     (match ty with
      | Types.RECORD (fields, unique) ->
	 let (fieldname, fieldty) = List.find (fun (fieldname, fieldty) ->
					fieldname = id) fields in
	 {exp=(); ty=fieldty}
      | _ ->
	 error "expected record" pos;
	 {exp=(); ty=Types.INT})
  | A.SubscriptVar (v, exp, pos) ->
     let {ty; _} = trans_var venv tenv v in
     (match ty with
      | Types.ARRAY (ty, unique) ->
	 ignore (check_int (trans_exp venv tenv exp) pos);
	 {exp=(); ty=ty}
      | _ ->
	 error "expected array" pos;
	 {exp=(); ty=Types.INT})

and trans_exp venv tenv exp =
  let rec trexp = function
    | A.VarExp v ->
       trans_var venv tenv v
    | A.NilExp ->
       {exp=(); ty=Types.NIL}
    | A.IntExp i ->
       {exp=(); ty=Types.INT}
    | A.StringExp (s, pos) ->
       {exp=(); ty=Types.STRING}
    | A.CallExp (func, args, pos) ->
       (match S.look func venv with
	| Env.FunEntry (formaltys, result) ->
	   let chktys formal arg =
	     if formal <> arg
	     then error "type mismatch" pos in
	   let argty exp = (trexp exp).ty in
	   List.iter2 chktys formaltys (List.map argty args);
	   {exp=(); ty=result}
	| _ ->
	   error "expected function" pos;
	   {exp=(); ty=Types.INT})
    | A.OpExp (l, op, r, pos) ->
       ignore (check_int (trexp l) pos);
       ignore (check_int (trexp r) pos);
       {exp=(); ty=Types.INT}
    | A.RecordExp (fields, tyid, pos) ->
       (match S.look tyid tenv with
	| Types.RECORD (fieldtys, unique) as ty ->
	   List.iter2 (fun (s1,exp,pos) (s,ty)->
	       if s1 <> s
	       then error ("expected field " ^ S.name s) pos;
	       let {ty=ty1; _} = trexp exp in
	       if ty1 <> ty
	       then error "type mismatch" pos) fields fieldtys;
	   {exp=(); ty=ty}
	| _ ->
	   error "expected record" pos;
	   {exp=(); ty=Types.RECORD([], ref ())}
       )
    | A.SeqExp (exps) ->
       List.fold_left (fun _ (exp, pos) ->
	   trexp exp) {exp=(); ty=Types.UNIT} exps
    | A.AssignExp (v, e, pos) ->
       ignore (trans_var venv tenv v);
       ignore (trexp e);
       {exp=(); ty=Types.UNIT}
    | A.IfExp (test, then', None, pos) ->
       ignore (check_int (trexp test) pos);
       trexp then'
    | A.IfExp (test, then', Some else', pos) ->
       ignore (check_int (trexp test) pos);
       let {ty=thenty; _} = trexp then' in
       let {ty=elsety; _} = trexp else' in
       if thenty <> elsety
       then error "type mismatch" pos;
       {exp=(); ty=thenty}
    | A.WhileExp (test, body, pos) ->
       ignore (check_int (trexp test) pos);
       let {ty=bodyty; _} = trexp body in
       if bodyty <> Types.UNIT
       then error "expected unit" pos;
       {exp=(); ty=Types.UNIT}
    | A.ForExp (var, escape, lo, hi, body, pos) ->
       ignore (check_int (trexp lo) pos);
       ignore (check_int (trexp hi) pos);
       let venv' = S.enter var (Env.VarEntry Types.INT) venv in
       let {ty=bodyty; _} = trans_exp venv' tenv body in
       if bodyty <> Types.UNIT
       then error "expected unit" pos;
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
	| Types.ARRAY (ty, unique) as aty ->
	   ignore (check_int (trexp size) pos);
	   let {ty=inity; _} = trexp init in
	   if inity <> ty
	   then error "type mismatch" pos;
	   {exp=(); ty=aty}
	| _ ->
	   error "expected array" pos;
	   {exp=(); ty=Types.ARRAY (Types.INT, ref ())}
       )

  in trexp exp

and trans_dec venv tenv = function
  | A.VarDec {A.vardec_name; vardec_ty=None; vardec_init; _} ->
     let {exp; ty} = trans_exp venv tenv vardec_init in
     (* TODO: constrain NIL to RECORD *)
     {venv=S.enter vardec_name (Env.VarEntry ty) venv; tenv}
  | A.VarDec {A.vardec_name; vardec_ty=Some(tyname, typos); vardec_init; _} ->
     let ty = S.look tyname tenv in
     let {exp; ty=expty} = trans_exp venv tenv vardec_init in
     (* TODO: constrain NIL to RECORD *)
     if ty <> expty
     then error "type mismatch" typos;
     {venv=S.enter vardec_name (Env.VarEntry ty) venv; tenv}
  | A.TypeDec tydecs ->
     let trtydec {venv; tenv} {A.tydec_name; tydec_ty; tydec_pos} =
       {venv; tenv=S.enter tydec_name (trans_ty tenv tydec_ty) tenv} in
     List.fold_left trtydec {venv;tenv} tydecs
  | A.FunctionDec [{A.fundec_name; fundec_params; fundec_result=Some(rt,pos); fundec_body; fundec_pos}] ->
     let resultty = S.look rt tenv in
     let trparam {A.name; escape; ty; pos} = (name, S.look ty tenv) in
     let params' = List.map trparam fundec_params in
     let venv' = S.enter fundec_name (Env.FunEntry ((List.map snd params'), resultty)) venv in
     let enterparam venv (name, ty) = S.enter name (Env.VarEntry ty) venv in
     let venv'' = List.fold_left enterparam venv' params' in
     let {ty=bodyty; _} = trans_exp venv'' tenv fundec_body in
     if bodyty <> resultty
     then error "type mismatch" pos;
     {venv=venv'; tenv}

and trans_ty tenv = function
  | A.NameTy (s, p) ->
     S.look s tenv
  | A.RecordTy fields ->
     let trfield {A.name; escape; ty; pos} = (name, S.look ty tenv) in
     Types.RECORD ((List.map trfield fields), ref ())
  | A.ArrayTy (s, p) ->
     Types.ARRAY ((S.look s tenv), ref ())

let trans_prog exp =
  ignore (trans_exp Env.base_venv Env.base_tenv exp)
