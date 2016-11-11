val linearize : Tree.stm -> Tree.stm list
val basic_blocks : Tree.stm list -> (Tree.stm list list * Temp.label)
val trace_schedule : (Tree.stm list list * Temp.label) -> Tree.stm list
