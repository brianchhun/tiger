type access
type frame

type frag =
    STRING of Temp.label * string
  | PROC of Tree.stm * frame
            
type register = string
  
val temp_map : register Temp.Table.t

val specialregs : Temp.temp list
val argregs : Temp.temp list
val calleesaves : Temp.temp list
val callersaves : Temp.temp list

val fp : Temp.temp
val sp : Temp.temp
val rv : Temp.temp
val ra : Temp.temp
val zero: Temp.temp

val word_size : int

val new_frame : Temp.label -> bool list -> frame
val name : frame -> Temp.label
val formals : frame -> access list
val alloc_local : frame -> bool -> access

val exp : access -> Tree.exp -> Tree.exp

val external_call : string -> Tree.exp list -> Tree.exp

val proc_entry_exit1 : frame -> Tree.stm -> Tree.stm
val proc_entry_exit2 : frame -> Assem.instr list -> Assem.instr list
val proc_entry_exit3 : frame -> Assem.instr list -> string * Assem.instr list * string
    
val string : Temp.label -> string -> string
