type igraph =
  {graph: Igraph.graph;
   tnode: Temp.temp -> Igraph.node;
   gtemp: Igraph.node -> Temp.temp;
   moves: (Igraph.node * Igraph.node) list}

type liveset = unit Temp.Table.t * Temp.temp list
                 
type livemap = liveset Flow.Graph.Table.t

val liveness : Flow.flowgraph -> livemap

val interference_graph : Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list)

val show : out_channel -> igraph -> unit
