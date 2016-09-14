module A = Absyn

let rec indent = function
    0 -> ""
  | i -> " " ^ indent (i-1)

let rec dolist d f = function
  | [] -> ""
  | [x] -> "\n" ^ f (d+1) x
  | x :: xs -> "\n" ^ f (d+1) x ^ "," ^ dolist d f xs

let opname = function
    A.PlusOp -> "PlusOp"
  | A.MinusOp -> "MinusOp"
  | A.TimesOp -> "TimesOp"
  | A.DivideOp -> "DivideOp"
  | A.EqOp -> "EqOp"
  | A.NeqOp -> "NeqOp"
  | A.LtOp -> "LtOp"
  | A.LeOp -> "LeOp"
  | A.GtOp -> "GtOp"
  | A.GeOp -> "GeOp"

let print_field d {A.name; escape; ty; pos} =
  indent d ^ "(" ^ (Symbol.name name) ^ "," ^ (string_of_bool !escape) ^ "," ^ (Symbol.name ty) ^ ")"

let rec print_var d = function
  | A.SimpleVar (s, p) ->
      indent d ^ "SimpleVar(" ^ (Symbol.name s) ^ ")"
  | A.FieldVar (v, s, p) ->
      indent d ^ "\nFieldVar(" ^ print_var (d+1) v ^ "\n," ^ indent (d+1) ^ (Symbol.name s) ^ ")"
  | A.SubscriptVar (v, e, p) ->
      indent d ^ "\nSubscriptVar(" ^ print_var (d+1) v ^ "\n," ^ print_exp (d+1) e ^ ")"

and print_exp d = function
  | A.VarExp v ->
      indent d ^ "\nVarExp(" ^ print_var (d+1) v ^ ")"
  | A.NilExp ->
      indent d ^ "NilExp"
  | A.IntExp i ->
      indent d ^ "IntExp(" ^ string_of_int i ^ ")"
  | A.StringExp (s, p) ->
      indent d ^ "StringExp(\"" ^ s ^ "\")"
  | A.CallExp (s, args, p) ->
      indent d ^ "CallExp(" ^ (Symbol.name s) ^ ",[" ^ dolist d print_exp args ^ "])"
  | A.OpExp (l, op, r, p) ->
      indent d ^ "OpExp(" ^ print_exp (d+1) l ^ "\n," ^ (opname op) ^ "\n," ^ print_exp (d+1) r ^ ")"
  | A.RecordExp (fields, typ, p) ->
      let print_recfield d (s, e, p) =
        indent d ^ "(" ^ (Symbol.name s) ^ "\n," ^ print_exp (d+1) e ^ ")" in
        indent d ^ "RecordExp(" ^ (Symbol.name typ) ^ "\n,[" ^ dolist d print_recfield fields ^ "])"
  | A.SeqExp exps ->
      indent d ^ "SeqExp[" ^ dolist d print_exp (List.map fst exps) ^ "]"
  | A.AssignExp (v, e, p) ->
      indent d ^ "\nAssignExp(" ^ print_var (d+1) v ^ "\n," ^ print_exp (d+1) e ^ ")"
  | A.IfExp (test, then', else', p) ->
      indent d ^ "\nIfExp(" ^ print_exp (d+1) test ^ "\n," ^
      print_exp (d+1) then' ^
      (match else' with Some e -> "\n," ^ print_exp (d+1) e | None -> "") ^ ")"
  | A.WhileExp (test, body, p) ->
      indent d ^ "\nWhileExp(" ^ print_exp (d+1) test ^ "\n," ^ print_exp (d+1) body ^ "\n," ^ string_of_int p ^ ")"
  | A.ForExp (v, b, lo, hi, body, p) ->
      indent d ^ "\nForExp(" ^ (Symbol.name v) ^ "," ^ (string_of_bool !b) ^ "\n," ^
      print_exp (d+1) lo ^ "\n," ^ print_exp (d+1) hi ^ "\n," ^
      print_exp (d+1) body ^ "\n" ^ ")"
  | A.BreakExp p ->
      indent d ^ "BreakExp"
  | A.LetExp (decs, body, p) ->
      indent d ^ "LetExp([" ^ dolist d print_dec decs ^ "\n]," ^ print_exp (d+1) body ^ ")"
  | A.ArrayExp (typ, size, init, p) ->
      indent d ^
      "ArrayExp(" ^ (Symbol.name typ) ^ "\n," ^ print_exp (d+1) size ^ "\n," ^ print_exp (d+1) init ^ ")"

and print_dec d = function
  | A.FunctionDec l ->
      indent d ^ "FunctionDec[" ^ dolist d print_fundec l ^ "]"
  | A.VarDec {A.vardec_name; vardec_escape; vardec_ty; vardec_init; vardec_pos} ->
      indent d ^ "VarDec(" ^ (Symbol.name vardec_name) ^ "," ^
      (string_of_bool !vardec_escape) ^ "," ^
      (match vardec_ty with Some (s, _) -> "Some(" ^ (Symbol.name s) ^ ")" | None -> "None") ^ ")" ^ "\n," ^
      print_exp (d+1) vardec_init ^ ")"
  | A.TypeDec l ->
      indent d ^ "TypeDec[" ^ dolist d print_tydec l ^ "]"

and print_ty d = function
  | A.NameTy (s, p) ->
      indent d ^ "NameTy(" ^ (Symbol.name s) ^ ")"
  | A.RecordTy fields ->
      indent d ^ "RecordTy[" ^ dolist d print_field fields ^ "]"
  | A.ArrayTy (s, p) ->
      indent d ^ "ArrayTy(" ^ (Symbol.name s) ^ ")"

and print_fundec d {A.fundec_name; fundec_params; fundec_result; fundec_body; fundec_pos} =
  indent d ^ "(" ^ (Symbol.name fundec_name) ^ ",[" ^ dolist d print_field fundec_params ^ "\n]," ^
  (match fundec_result with Some (s, _) -> "Some(" ^ (Symbol.name s) ^ ")" | None -> "None") ^
  "\n," ^ print_exp (d+1) fundec_body ^ ")"

and print_tydec d {A.tydec_name; tydec_ty; tydec_pos} =
  indent d ^ "(" ^ (Symbol.name tydec_name) ^ "\n," ^ print_ty (d+1) tydec_ty ^ ")"

let print e = print_exp 0 e
