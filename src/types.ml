type unique = unit ref

type t = RECORD of (Symbol.t * t) list * unique
       | NIL
       | INT
       | STRING
       | ARRAY of t * unique
       | NAME of Symbol.t * t option ref
       | UNIT

let rec string_of_ty = function
	| RECORD (fields, unique) ->
		 "{" ^ (String.concat ", " (List.map (fun (s, ty) -> Symbol.name s ^ ":" ^ string_of_ty ty) fields)) ^ "}"
	| NIL-> "nil"
	| INT -> "int"
	| STRING -> "string"
	| ARRAY (ty, unique) -> string_of_ty ty ^ " array"
	| NAME (s, ty) -> Symbol.name s
	| UNIT -> "unit"
