type allocation = Frame.register Temp.Table.t

let alloc instrs frame =
  let (flowgraph, flownodes) = Make_graph.instrs2graph instrs in
  let (igraph, liveouts) = Liveness.interference_graph flowgraph in
  let (allocation, spills) = Color.color igraph Frame.temp_map Frame.registers in
    (instrs, allocation)
