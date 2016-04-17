type access
type ty
type enventry = VarEntry of ty
	      | FunEntry of ty list * ty

val base_tenv : ty Symbol.table
val base_venv : enventry Symbol.table
