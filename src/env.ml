type access
type enventry = VarEntry of Types.t
	      | FunEntry of Types.t list * Types.t

let base_tenv =
  Symbol.enter (Symbol.symbol "int") Types.INT
  @@ Symbol.enter (Symbol.symbol "string") Types.STRING
  @@ Symbol.empty

let base_venv =
  Symbol.enter (Symbol.symbol "print") (FunEntry ([Types.STRING], Types.UNIT))
  @@ Symbol.enter (Symbol.symbol "flush") (FunEntry ([], Types.UNIT))
  @@ Symbol.enter (Symbol.symbol "getchar") (FunEntry ([], Types.STRING))
  @@ Symbol.enter (Symbol.symbol "ord") (FunEntry ([Types.STRING], Types.INT))
  @@ Symbol.enter (Symbol.symbol "chr") (FunEntry ([Types.INT], Types.STRING))
  @@ Symbol.enter (Symbol.symbol "size") (FunEntry ([Types.STRING], Types.INT))
  @@ Symbol.enter (Symbol.symbol "substring") (FunEntry ([Types.STRING; Types.INT; Types.INT], Types.STRING))
  @@ Symbol.enter (Symbol.symbol "concat") (FunEntry ([Types.STRING; Types.STRING], Types.STRING))
  @@ Symbol.enter (Symbol.symbol "not") (FunEntry ([Types.INT], Types.INT))
  @@ Symbol.enter (Symbol.symbol "exit") (FunEntry ([Types.INT], Types.UNIT))
  @@ Symbol.empty
