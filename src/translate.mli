type level
type access
type exp
type frag = Frame.frag

val outermost : level
val new_level : level -> Temp.label -> bool list -> level
val formals : level -> access list
val alloc_local : level -> bool -> access

val un_ex : exp -> Tree.exp
val un_cx : exp -> (Temp.label -> Temp.label -> Tree.stm)
val un_nx : exp -> Tree.stm

val unit_exp : exp

val simple_var : access -> level -> exp
val subscript_var : exp -> exp -> exp
val field_var : exp -> int -> exp

val nil_exp : exp
val int_exp : int -> exp

val plus_op_exp : exp -> exp -> exp
val minus_op_exp : exp -> exp -> exp
val mul_op_exp : exp -> exp -> exp
val div_op_exp : exp -> exp -> exp

val eq_op_exp : exp -> exp -> exp
val ne_op_exp : exp -> exp -> exp
val lt_op_exp : exp -> exp -> exp
val le_op_exp : exp -> exp -> exp
val gt_op_exp : exp -> exp -> exp
val ge_op_exp : exp -> exp -> exp

val string_eq_op_exp : exp -> exp -> exp
val string_ne_op_exp : exp -> exp -> exp

val if_exp2 : exp -> exp -> exp
val if_exp3 : exp -> exp -> exp -> exp

val string_exp : string -> exp

val record_exp : exp list -> exp
val array_exp : exp -> exp -> exp

val seq_exp : exp list -> exp

val while_exp : exp -> exp -> Temp.label -> exp
val break_exp : Temp.label -> exp

val call_exp : level -> level -> Temp.label -> exp list -> exp

val assign_exp : exp -> exp -> exp
val let_exp : exp list -> exp -> exp

val proc_entry_exit : level -> exp -> unit
val get_result : unit -> frag list
