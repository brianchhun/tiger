module A = Absyn

let rec indent = function
    0 -> ()
  | i -> print_string " "; indent (i-1)

let rec dolist d f = function
  | [] -> ()
  | [x] -> print_newline(); f (d+1) x
  | x :: xs ->
     print_newline();
     f (d+1) x;
     print_string ",";
     dolist d f xs

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
  indent d;
  print_string "(";
  print_string (Symbol.name name);
  print_string ",";
  print_string (string_of_bool !escape);
  print_string ",";
  print_string (Symbol.name ty);
  print_string ")"

let rec print_var d = function
  | A.SimpleVar (s, p) ->
     indent d;
     print_string "SimpleVar(";
     print_string (Symbol.name s);
     print_string ")"
  | A.FieldVar (v, s, p) ->
     indent d;
     print_endline "FieldVar(";
     print_var (d+1) v;
     print_endline ",";
     indent (d+1);
     print_string (Symbol.name s);
     print_string ")"
  | A.SubscriptVar (v, e, p) ->
     indent d;
     print_endline "SubscriptVar(";
     print_var (d+1) v;
     print_endline ",";
     print_exp (d+1) e;
     print_string ")"

and print_exp d = function
  | A.VarExp v ->
     indent d;
     print_endline "VarExp(";
     print_var (d+1) v;
     print_string ")"
  | A.NilExp ->
     indent d;
     print_string "NilExp"
  | A.IntExp i ->
     indent d;
     print_string "IntExp(";
     print_int i;
     print_string ")"
  | A.StringExp (s, p) ->
     indent d;
     print_string "StringExp(\"";
     print_string s;
     print_string "\")"
  | A.CallExp (s, args, p) ->
     indent d;
     print_string "CallExp(";
     print_string (Symbol.name s);
     print_string ",[";
     dolist d print_exp args;
     print_string "])"
  | A.OpExp (l, op, r, p) ->
     indent d;
     print_string "OpExp(";
     print_string (opname op);
     print_endline ",";
     print_exp (d+1) l;
     print_endline ",";
     print_exp (d+1) r;
     print_string ")"
  | A.RecordExp (fields, typ, p) ->
     let print_recfield d (s, e, p) =
       indent d;
       print_string "(";
       print_string (Symbol.name s);
       print_endline ",";
       print_exp (d+1) e;
       print_string ")" in
     indent d;
     print_string "RecordExp(";
     print_string (Symbol.name typ);
     print_endline ",[";
     dolist d print_recfield fields;
     print_string "])"
  | A.SeqExp exps ->
     indent d;
     print_string "SeqExp[";
     dolist d print_exp (List.map fst exps);
     print_string "]"
  | A.AssignExp (v, e, p) ->
     indent d;
     print_endline "AssignExp(";
     print_var (d+1) v;
     print_endline ",";
     print_exp (d+1) e;
     print_string ")"
  | A.IfExp (test, then', else', p) ->
     indent d;
     print_endline "IfExp(";
     print_exp (d+1) test;
     print_endline ",";
     print_exp (d+1) then';
     (match else' with
	Some e -> print_endline ","; print_exp (d+1) e
      | None -> ());
     print_string ")"
  | A.WhileExp (test, body, p) ->
     indent d;
     print_endline "WhileExp(";
     print_exp (d+1) test;
     print_endline ",";
     print_exp (d+1) body;
     print_string ")"
  | A.ForExp (v, b, lo, hi, body, p) ->
     indent d;
     print_endline "ForExp(";
     print_string (Symbol.name v);
     print_string ",";
     print_string (string_of_bool !b);
     print_endline ",";
     print_exp (d+1) lo;
     print_endline ",";
     print_exp (d+1) hi;
     print_endline ",";
     print_exp (d+1) body;
     print_endline ",";
     print_string ")"
  | A.BreakExp p ->
     indent d;
     print_string "BreakExp"
  | A.LetExp (decs, body, p) ->
     indent d;
     print_string "LetExp([";
     dolist d print_dec decs;
     print_endline "],";
     print_exp (d+1) body;
     print_string ")"
  | A.ArrayExp (typ, size, init, p) ->
     indent d;
     print_string "ArrayExp(";
     print_string (Symbol.name typ);
     print_endline ",";
     print_exp (d+1) size;
     print_endline ",";
     print_exp (d+1) init;
     print_string ")"

and print_dec d = function
  | A.FunctionDec l ->
     indent d;
     print_string "FunctionDec[";
     dolist d print_fundec l;
     print_string "]"
  | A.VarDec {A.vardec_name; vardec_escape; vardec_ty; vardec_init; vardec_pos} ->
     indent d;
     print_string "VarDec(";
     print_string (Symbol.name vardec_name);
     print_string ",";
     print_string (string_of_bool !vardec_escape);
     print_string ",";
     (match vardec_ty with
	 Some (s, _) -> print_string "Some("; print_string (Symbol.name s); print_string ")"
       | None -> print_string "None");
     print_string ")";
     print_endline ",";
     print_exp (d+1) vardec_init;
     print_string ")"
  | A.TypeDec l ->
     indent d;
     print_string "TypeDec[";
     dolist d print_tydec l;
     print_string "]"

and print_ty d = function
  | A.NameTy (s, p) ->
     indent d;
     print_string "NameTy(";
     print_string (Symbol.name s);
     print_string ")"
  | A.RecordTy fields ->
     indent d;
     print_string "RecordTy[";
     dolist d print_field fields;
     print_string "]"
  | A.ArrayTy (s, p) ->
     indent d;
     print_string "ArrayTy(";
     print_string (Symbol.name s);
     print_string ")"

and print_fundec d {A.fundec_name; fundec_params; fundec_result; fundec_body; fundec_pos} =
  indent d;
  print_string "(";
  print_string (Symbol.name fundec_name);
  print_string ",[";
  dolist d print_field fundec_params;
  print_endline "],";
  (match fundec_result with
     Some (s, _) -> print_string "Some("; print_string (Symbol.name s); print_string ")"
   | None -> print_string "None");
  print_endline ",";
  print_exp (d+1) fundec_body;
  print_string ")"

and print_tydec d {A.tydec_name; tydec_ty; tydec_pos} =
  indent d;
  print_string "(";
  print_string (Symbol.name tydec_name);
  print_endline ",";
  print_ty (d+1) tydec_ty;
  print_string ")"

let print e = print_exp 0 e; print_newline()
