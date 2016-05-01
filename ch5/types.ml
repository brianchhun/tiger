type unique = unit ref

type ty = RECORD of (Symbol.symbol * ty) list * unique
	| NIL
	| INT
	| STRING
	| ARRAY of ty * unique
	| NAME of Symbol.symbol * ty option ref
	| UNIT

let rec string_of_ty = function
	| RECORD (fields, unique) ->
		 "{" ^ (String.concat ", " (List.map (fun (s, ty) -> Symbol.name s ^ ":" ^ string_of_ty ty) fields)) ^ "}"
	| NIL-> "nil"
	| INT -> "int"
	| STRING -> "string"
	| ARRAY (ty, unique) -> string_of_ty ty ^ " array"
	| NAME (s, ty) -> "name" (* TODO *)
	| UNIT -> "unit"
