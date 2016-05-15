type t = string * int
let nextsym = ref 0
let size_hint = 128
let hash_table : (string, int) Hashtbl.t = Hashtbl.create size_hint
let symbol name =
  try
    (name, Hashtbl.find hash_table name)
  with
    Not_found -> let i = !nextsym
		 in nextsym := i+1;
		    Hashtbl.add hash_table name i;
		    (name, i)

let name = function
  | (s,i) -> s

module Table =
  Map.Make(struct
	    type symbol = t
	    type t = symbol
	    let compare = compare end)
type 'a table = 'a Table.t
let empty = Table.empty
let enter = Table.add
let look s t = try Some (Table.find s t) with Not_found -> None
