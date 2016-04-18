module A = Absyn

type venv = Env.enventry Symbol.table
type tenv = Env.ty Symbol.table
type expty = {exp: Translate.exp; ty: Types.ty}

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
  | A.SimpleVar (s, pos) ->
     {exp=(); ty=Types.UNIT}
  | A.FieldVar (v, s, pos) ->
     {exp=(); ty=Types.UNIT}
  | A.SubscriptVar (v, e, pos) ->
     {exp=(); ty=Types.UNIT}

let rec trans_exp venv tenv = function
  | A.VarExp v ->
     trans_var venv tenv v
  | A.IntExp i ->
     {exp=(); ty=Types.INT}
  | A.NilExp ->
     {exp=(); ty=Types.NIL}
  | A.StringExp (s, pos) ->
     {exp=(); ty=Types.STRING}
  | A.CallExp (s, args, pos) ->
     {exp=(); ty=Types.UNIT}
  | A.OpExp (l, op, r, pos) ->
     ignore (check_int (trans_exp venv tenv l) pos);
     ignore (check_int (trans_exp venv tenv r) pos);
     {exp=(); ty=Types.INT}
  | A.RecordExp (fields, ty, pos) ->
     let fieldstys = List.map (fun (s, e, pos) ->
			 let {ty; _} = trans_exp venv tenv e in
			 (s, ty)) fields in
     {exp=(); ty=Types.RECORD (fieldstys, ref ())}
  | A.SeqExp (exps) ->
     let trseq res (exp,p) = trans_exp venv tenv exp in
     List.fold_left trseq {exp=(); ty=Types.UNIT} exps
  | A.AssignExp (v, e, pos) ->
     ignore (trans_var venv tenv v);
     ignore (trans_exp venv tenv e);
     {exp=(); ty=Types.UNIT}
  | A.IfExp (test, then', Some else', pos) ->
     ignore (check_int (trans_exp venv tenv test) pos);
     let {ty=thenty; _ } = trans_exp venv tenv then' in
     let {ty=elsety; _ } = trans_exp venv tenv else' in
     if thenty <> elsety then error "expected equal types" pos;
     {exp=(); ty=thenty}
  | A.IfExp (test, then', None, pos) ->
     ignore (check_int (trans_exp venv tenv test) pos);
     ignore (check_unit (trans_exp venv tenv then') pos);
     {exp=(); ty=Types.UNIT}
  | A.WhileExp (test, body, pos) ->
     ignore (check_int (trans_exp venv tenv test) pos);
     ignore (check_unit (trans_exp venv tenv body) pos);
     {exp=(); ty=Types.UNIT}
  | A.ForExp (var, escape, lo, hi, body, pos) ->
     ignore (check_int (trans_exp venv tenv lo) pos);
     ignore (check_int (trans_exp venv tenv hi) pos);
     let tenv' = Symbol.enter var Types.INT tenv in
     ignore (check_unit (trans_exp venv tenv' body) pos);
     {exp=(); ty=Types.UNIT}
  | A.BreakExp p ->
     {exp=(); ty=Types.UNIT}
  | A.LetExp (decs, body, p) ->
     {exp=(); ty=Types.UNIT}
  | A.ArrayExp (tyid, size, init, pos) ->
     ignore (check_int (trans_exp venv tenv size) pos);
     ignore (trans_exp venv tenv init);
     match Symbol.look tyid tenv with
     | Some ty -> {exp=(); ty=Types.ARRAY (ty, ref ())}
     | None -> error ("undefined type " ^ Symbol.name tyid) pos;
	       {exp=(); ty=Types.ARRAY (Types.INT, ref ())}
		 
let rec trans_dec venv tenv = function
  | A.FunctionDec fundecs -> ()
  | A.VarDec vardec -> ()
  | A.TypeDec typedecs -> ()

let rec trans_ty tenv = function
  | A.NameTy (s, p) -> ()
  | A.RecordTy fields -> ()
  | A.ArrayTy (s, p) -> ()
					  
let trans_prog = trans_exp Env.base_venv Env.base_tenv
