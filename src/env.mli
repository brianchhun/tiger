type access
type enventry =
    VarEntry of Translate.access * Types.t
  | FunEntry of Translate.level * Temp.label * Types.t list * Types.t

val base_tenv : Types.t Symbol.table
val base_venv : enventry Symbol.table
