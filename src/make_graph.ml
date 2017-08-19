open ExtList

let instrs2graph instrs =
  let control = Graph.new_graph () in
  let label_map = List.fold_left
      (fun map instr ->
         match instr with
           Assem.LABEL {Assem.assem; lab} ->
             Symbol.enter lab (Graph.new_node control) map
         | _ -> map)
      Symbol.empty
      instrs in
  let label_node lab =
    Option.get (Symbol.look lab label_map) in
  let mk_succ node succ =
    List.iter (Graph.mk_edge node) succ in
  let rec instrs2graph' = function
      [] ->
        ({Flow.
           control;
           def=Graph.Table.empty;
           use=Graph.Table.empty;
           ismove=Graph.Table.empty},
         [])
    | Assem.OPER {Assem.assem; src; dst; jump} :: instrs ->
        let node = Graph.new_node control in
        let ({Flow.control; def; use; ismove}, nodes) = instrs2graph' instrs in
        let succ = Option.map_default (List.map label_node) (List.take 1 nodes) jump in
          mk_succ node succ;
          ({Flow.
             control;
             def=Graph.Table.add node (List.sort dst) def;
             use=Graph.Table.add node (List.sort src) use;
             ismove=Graph.Table.add node false ismove},
           node :: nodes)
    | Assem.LABEL {Assem.assem; lab} :: instrs ->
        let node = label_node lab in
        let ({Flow.control; def; use; ismove}, nodes) = instrs2graph' instrs in
          mk_succ node (List.take 1 nodes);
          ({Flow.
             control;
             def=Graph.Table.add node [] def;
             use=Graph.Table.add node [] use;
             ismove=Graph.Table.add node false ismove},
           node :: nodes)
    | Assem.MOVE {Assem.assem; src; dst} :: instrs ->
        let node = Graph.new_node control in
        let ({Flow.control; def; use; ismove}, nodes) = instrs2graph' instrs in
          mk_succ node (List.take 1 nodes);
          ({Flow.
             control;
             def=Graph.Table.add node [dst] def;
             use=Graph.Table.add node [src] use;
             ismove=Graph.Table.add node true ismove},
           node :: nodes) in
    instrs2graph' instrs
