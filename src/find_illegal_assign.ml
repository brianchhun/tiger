open Absyn

type env = bool Symbol.table

let rec traverse_var env = function
    SimpleVar (id, pos) ->
      (match Symbol.look id env with Some legal -> legal | None -> true)
  | FieldVar (var, id, pos) ->
      traverse_var env var
  | SubscriptVar (var, i, pos) ->
      traverse_exp env i;
      traverse_var env var
and traverse_exp env = function
    VarExp _ -> ()
  | NilExp -> ()
  | IntExp _ -> ()
  | StringExp _-> ()
  | CallExp (name, args, pos) ->
      List.iter (fun arg -> traverse_exp env arg) args
  | OpExp (l, op, r, pos) ->
      traverse_exp env l;
      traverse_exp env r
  | RecordExp (fields, ty_id, pos) ->
      List.iter (fun (name, exp, pos)  -> traverse_exp env exp) fields
  | SeqExp (exps) ->
      List.iter (fun (exp, pos) -> traverse_exp env exp) exps
  | AssignExp (var, exp, legal, pos) ->
      legal := traverse_var env var;
      traverse_exp env exp
  | IfExp (test, conseq, alt, pos) ->
      traverse_exp env test;
      traverse_exp env conseq;
      (match alt with Some alt' -> traverse_exp env alt' | None -> ())
  | WhileExp (test, body, pos) ->
      traverse_exp env test;
      traverse_exp env body
  | ForExp (var, escape, lo, hi, body, pos) ->
      traverse_exp env lo;
      traverse_exp env hi;
      traverse_exp (Symbol.enter var false env) body
  | BreakExp _ -> ()
  | LetExp (decs, body, pos) ->
      let env' = List.fold_left (fun env' dec -> traverse_dec env' dec) env decs in
        traverse_exp env' body
  | ArrayExp (ty_id, size, init, pos) ->
      traverse_exp env size;
      traverse_exp env init
and traverse_dec env = function
    VarDec {vardec_name; vardec_escape; vardec_init; _} ->
      traverse_exp env vardec_init;
      Symbol.enter vardec_name true env
  | TypeDec _ -> env
  | FunctionDec fundecs ->
      List.iter
        (fun {fundec_body; fundec_params; _} ->
           let env' = List.fold_left
               (fun env {name; escape; _} -> Symbol.enter name true env)
               env
               fundec_params in
             traverse_exp env' fundec_body)
        fundecs;
      env

let find_illegal_assign prog =
  traverse_exp Symbol.empty prog
