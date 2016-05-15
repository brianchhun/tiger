type access
type enventry = VarEntry of Types.ty
	      | FunEntry of Types.ty list * Types.ty

val base_tenv : Types.ty Symbol.table
val base_venv : enventry Symbol.table
