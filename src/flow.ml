type flowgraph =
  {control: Graph.graph;
   def: Temp.temp list Graph.Table.t;
   use: Temp.temp list Graph.Table.t;
   ismove: bool Graph.Table.t}

module Graph = Graph
