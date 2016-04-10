module A = Absyn

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

let rec indent = function
    0 -> ()
  | i -> print_string " "; indent (i-1)

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
     print_string "NilExp";
  | A.IntExp i ->
     indent d;
     print_string "IntExp(";
     print_int i;
     print_string ")";
  | A.StringExp (s, p) ->
     indent d;
     print_string "StringExp(\"";
     print_string s;
     print_string "\")";
  | A.CallExp _ -> ()
  | A.OpExp _ -> ()
  | A.RecordExp _ -> ()
  | A.SeqExp _ -> ()
  | A.AssignExp _ -> ()
  | A.IfExp _ -> ()
  | A.WhileExp _ -> ()
  | A.ForExp _ -> ()
  | A.BreakExp _ -> ()
  | A.LetExp _ -> ()
  | A.ArrayExp _ -> ()
		       
let print e = print_exp 0 e; flush stdout
