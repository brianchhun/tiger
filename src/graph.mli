type graph
type node

val nodes: graph -> node list
val succ: node -> node list
val pred: node -> node list
val adj: node -> node list   (* succ+pred *)
val eq: node -> node -> bool

val new_graph: unit -> graph
val new_node : graph -> node
exception GraphEdge

val mk_edge: node -> node -> unit
val rm_edge: node -> node -> unit

val nodename: node->string  (* for debugging only *)

module Table : Map.S with type key = node
