type temp
val new_temp : unit -> temp
val string_of_temp : temp -> string

type label = Symbol.t
val new_label : unit -> label
val named_label : string -> label
val string_of_label : label -> string

module Table : Map.S with type key = temp
