type access
type enventry = VarEntry of Types.t
	      | FunEntry of Types.t list * Types.t

val base_tenv : Types.t Symbol.table
val base_venv : enventry Symbol.table
