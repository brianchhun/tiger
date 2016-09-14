type access
type frame

type frag =
    STRING of Temp.label * string
  | PROC of Tree.stm * frame

val fp : Tree.exp
val word_size : int

val rv : Temp.temp

val new_frame : Temp.label -> bool list -> frame
val name : frame -> Temp.label
val formals : frame -> access list
val alloc_local : frame -> bool -> access

val exp : access -> Tree.exp -> Tree.exp

val external_call : string -> Tree.exp list -> Tree.exp

val proc_entry_exit1 : frame -> Tree.stm -> Tree.stm
