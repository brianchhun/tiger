type t = Arity_mismatch of int * int
       | Array_type_mismatch
       | Duplicate_type_declaration of string
       | Duplicate_function_declaration of string
       | Illegal_break
       | Illegal_cycle_in_type_declaration
       | Illegal_comparison of string * string
       | Parse_error of string
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
       | Variable_cannot_be_assigned of string

val any_errors : bool ref
val line_num : int ref
val line_pos : int list ref
val error : int -> t -> unit
exception Error of string
val impossible : string -> 'a  (* raises Error *)
val parse_error : string -> unit
val reset : unit -> unit
