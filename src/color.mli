type allocation = Frame.register Temp.Table.t

val color : Liveness.igraph -> (Igraph.node -> int) -> allocation -> Frame.register list -> allocation * Temp.temp list
