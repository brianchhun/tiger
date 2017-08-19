type allocation = Frame.register Temp.Table.t

val alloc : Assem.instr list -> Frame.frame -> Assem.instr list * allocation
