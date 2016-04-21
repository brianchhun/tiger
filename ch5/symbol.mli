type symbol
val symbol : string -> symbol
val name : symbol -> string
type 'a table
val empty : 'a table
val enter : symbol -> 'a -> 'a table -> 'a table
val look : symbol -> 'a table -> 'a
