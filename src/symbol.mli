type t
val symbol : string -> t
val name : t -> string
type 'a table
val empty : 'a table
val enter : t -> 'a -> 'a table -> 'a table
val look : t -> 'a table -> 'a option
