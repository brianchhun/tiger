type allocation = Frame.register Temp.Table.t

val color : Liveness.igraph -> allocation -> Frame.register list -> allocation * Temp.temp list
