module A = Absyn

type depth = int
type escape_env = (depth * bool ref) Symbol.table

let enter id ((depth, escape) as e) env =
  escape := false;
  Symbol.enter id e env

let escape id depth env =
  match Symbol.look id env with
    Some (depth', escape) when depth > depth' ->
      escape := true
  | _ -> ()

let rec traverse_var env d = function
    A.SimpleVar (id, pos) ->
      escape id d env
  | A.FieldVar (var, id, pos) ->
      traverse_var env d var
  | A.SubscriptVar (var, i, pos) ->
      traverse_var env d var;
      traverse_exp env d i
and traverse_exp env d = function
    A.VarExp var ->
      traverse_var env d var
  | A.NilExp -> ()
  | A.IntExp _ -> ()
  | A.StringExp _-> ()
  | A.CallExp (name, args, pos) ->
      List.iter (fun arg -> traverse_exp env d arg) args
  | A.OpExp (l, op, r, pos) ->
      traverse_exp env d l;
      traverse_exp env d r
  | A.RecordExp (fields, ty_id, pos) ->
      List.iter (fun (name, exp, pos)  -> traverse_exp env d exp) fields
  | A.SeqExp (exps) ->
      List.iter (fun (exp, pos) -> traverse_exp env d exp) exps
  | A.AssignExp (var, exp, pos) ->
      traverse_var env d var;
      traverse_exp env d exp
  | A.IfExp (test, conseq, alt, pos) ->
      traverse_exp env d test;
      traverse_exp env d conseq;
      (match alt with Some alt' -> traverse_exp env d alt' | None -> ())
  | A.WhileExp (test, body, pos) ->
      traverse_exp env d test;
      traverse_exp env d body
  | A.ForExp (var, escape, lo, hi, body, pos) ->
      let env' = enter var (d , escape) env in
        traverse_exp env' d lo;
        traverse_exp env' d hi;
        traverse_exp env' d body
  | A.BreakExp _ -> ()
  | A.LetExp (decs, body, pos) ->
      let env' = List.fold_left (fun env' dec -> traverse_dec env' d dec) env decs in
          traverse_exp env' d body
  | A.ArrayExp (ty_id, size, init, pos) ->
      traverse_exp env d size;
      traverse_exp env d init
and traverse_dec env d = function
    A.VarDec {A.vardec_name; vardec_escape; vardec_init; _} ->
      traverse_exp env d vardec_init;
      enter vardec_name (d, vardec_escape) env
  | A.TypeDec _ -> env
  | A.FunctionDec fundecs ->
      List.iter
        (fun {A.fundec_body; fundec_params; _} ->
           let env' = List.fold_left
               (fun env {A.name; escape; _} ->
                  enter name (d + 1, escape) env)
               env
               fundec_params in
           traverse_exp env' (d + 1)  fundec_body)
        fundecs;
      env

let find_escape prog =
  traverse_exp Symbol.empty 0 prog
