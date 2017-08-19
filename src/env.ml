type access
type enventry =
    VarEntry of Translate.access * Types.t
  | FunEntry of Translate.level * Temp.label * Types.t list * Types.t

let base_tenv =
  Symbol.enter (Symbol.symbol "int") Types.INT
  @@ Symbol.enter (Symbol.symbol "string") Types.STRING
  @@ Symbol.empty

let base_venv =
  Symbol.enter (Symbol.symbol "print") (FunEntry (Translate.outermost, Temp.named_label "print", [Types.STRING], Types.UNIT))
  @@ Symbol.enter (Symbol.symbol "flush") (FunEntry (Translate.outermost, Temp.new_label (), [], Types.UNIT))
  @@ Symbol.enter (Symbol.symbol "getchar") (FunEntry (Translate.outermost, Temp.new_label (), [], Types.STRING))
  @@ Symbol.enter (Symbol.symbol "ord") (FunEntry (Translate.outermost, Temp.new_label (), [Types.STRING], Types.INT))
  @@ Symbol.enter (Symbol.symbol "chr") (FunEntry (Translate.outermost, Temp.new_label (), [Types.INT], Types.STRING))
  @@ Symbol.enter (Symbol.symbol "size") (FunEntry (Translate.outermost, Temp.new_label (), [Types.STRING], Types.INT))
  @@ Symbol.enter (Symbol.symbol "substring") (FunEntry (Translate.outermost, Temp.new_label (), [Types.STRING; Types.INT; Types.INT], Types.STRING))
  @@ Symbol.enter (Symbol.symbol "concat") (FunEntry (Translate.outermost, Temp.new_label (), [Types.STRING; Types.STRING], Types.STRING))
  @@ Symbol.enter (Symbol.symbol "not") (FunEntry (Translate.outermost, Temp.new_label (), [Types.INT], Types.INT))
  @@ Symbol.enter (Symbol.symbol "exit") (FunEntry (Translate.outermost, Temp.new_label (), [Types.INT], Types.UNIT))
  @@ Symbol.empty
