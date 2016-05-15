type t = Arity_mismatch of int * int
       | Array_type_mismatch
       | Duplicate_type_declaration of string
       | Duplicate_function_declaration of string
       | Illegal_break
       | Illegal_cycle_in_type_declaration
       | Illegal_comparison of string * string
       | Record_field_mismatch of string * string
       | Record_type_mismatch
       | Type_mismatch of string * string
       | Unconstrained_nil
       | Undefined_array of string
       | Undefined_function of string
       | Undefined_record of string
       | Undefined_record_field of string
       | Undefined_variable of string
       | Undefined_type of string

let string_of_error_msg = function
  | Arity_mismatch (s, t)         	-> "expected " ^ (string_of_int s) ^
					     " arguments but got " ^ (string_of_int t)
  | Array_type_mismatch			-> "different array types"
  | Duplicate_type_declaration s	-> "duplicate type declaration " ^ s
  | Duplicate_function_declaration s	-> "duplicate function declaration " ^ s
  | Illegal_break                       -> "breaks must be nested inside a for or while statement"
  | Illegal_cycle_in_type_declaration	-> "illegal cycle in mutually recursive type declaration"
  | Illegal_comparison (s, t)		-> "illegal comparison of " ^ s ^ " to " ^ t
  | Record_field_mismatch (s, t)        -> "expected record field " ^ s ^ " but got " ^ t
  | Record_type_mismatch		-> "different record types"
  | Type_mismatch (s, t)		-> "expected type " ^ s ^ " but got type " ^ t
  | Unconstrained_nil			-> "nil must be constrained to record"
  | Undefined_array s			-> "undefined array " ^ s
  | Undefined_function s		-> "undefined function " ^ s
  | Undefined_record s			-> "undefined record " ^ s
  | Undefined_record_field s		-> "undefined record field " ^ s
  | Undefined_type s			-> "undefined type " ^ s
  | Undefined_variable s		-> "undefined variable " ^ s

let any_errors = ref false
let line_num = ref 1
let line_pos = ref [-1]
let error pos errmsg =
  let rec look n = function
    | a :: rest ->
       if a<pos
       then Printf.printf "%d.%d" n (pos-a)
       else look (n-1) rest
    | _ ->  print_string "0.0" in
  any_errors := true;
  look !line_num !line_pos;
  print_string ":";
  print_endline (string_of_error_msg errmsg)
		
exception Error of string
let impossible msg = raise (Error msg)

let reset () =
  any_errors := false;
  line_num := 1;
  line_pos := [1];

