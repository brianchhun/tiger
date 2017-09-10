type allocation = Frame.register Temp.Table.t

type replace =
  {replacement: Temp.temp Temp.Table.t;
   instrs: Assem.instr list;
   temps: Temp.temp list}

let rewrite_program spilled frame instrs =
  let mem = List.fold_left
      (fun mem t -> Temp.Table.add t (Frame.alloc_local frame true) mem)
      Temp.Table.empty
      spilled in
  let replace temps instr =
    List.fold_right
      (fun t replace ->
         if List.mem t spilled then
           let t' = Temp.new_temp () in
             {replacement = Temp.Table.add t t' replace.replacement;
              instrs = instr t t' @ replace.instrs;
              temps = t' :: replace.temps}
         else
           replace)
      temps
      {replacement = Temp.Table.empty; instrs = []; temps = []} in
  let replace_src temps = 
    replace temps
      (fun t t' ->
         Codegen.codegen
           frame
           (Tree.MOVE (Tree.TEMP t', Frame.exp (Temp.Table.find t mem) (Tree.TEMP Frame.fp)))) in
  let replace_dst temps = 
    replace temps
      (fun t t' ->
         Codegen.codegen
           frame
           (Tree.MOVE (Frame.exp (Temp.Table.find t mem) (Tree.TEMP Frame.fp), Tree.TEMP t'))) in
  let replace_temps temps tab =
    List.map
      (fun t -> try Temp.Table.find t tab with Not_found -> t)
      temps in
  let rec loop = function
      [] -> ([], [])
    | Assem.OPER {Assem.assem; dst; src; jump} :: instrs ->
        let src_replace = replace_src src in
        let dst_replace = replace_dst dst in
        let instr = Assem.OPER {
            Assem.assem;
            src = replace_temps src src_replace.replacement;
            dst = replace_temps dst dst_replace.replacement;
            jump;
          } in
        let (temps, instrs') = loop instrs in
          (src_replace.temps @ dst_replace.temps @ temps,
           src_replace.instrs @ [instr] @ dst_replace.instrs @ instrs')
    | Assem.MOVE {Assem.assem; dst; src} :: instrs ->
        let src_replace = replace_src [src] in
        let dst_replace = replace_dst [dst] in
        let instr = Assem.MOVE {
            Assem.assem;
            src = List.hd (replace_temps [src] src_replace.replacement);
            dst = List.hd (replace_temps [dst] dst_replace.replacement);
          } in
        let (temps, instrs') = loop instrs in
          (src_replace.temps @ dst_replace.temps @ temps,
           src_replace.instrs @ [instr] @ dst_replace.instrs @ instrs')
    | (Assem.LABEL _ as instr) :: instrs ->
        let (temps, instrs') = loop instrs in (temps, instr :: instrs') in
    loop instrs

let alloc instrs frame =
  let rec loop instrs rewrite_temps =
    let ({Flow.def; use; _} as flowgraph, flownodes) = Make_graph.instrs2graph instrs in
    let ({Liveness.gtemp; _} as igraph, liveouts) = Liveness.interference_graph flowgraph in
    let spill_cost =
      let usedefs = List.fold_left
          (fun tab fnode ->
             let tab' = List.fold_left
                 (fun tab t ->
                    let (nuse, ndef) = try Temp.Table.find t tab with Not_found -> (0, 0) in
                      Temp.Table.add t (nuse+1, ndef) tab)
                 tab
                 (Flow.Graph.Table.find fnode use) in
               List.fold_left
                 (fun tab t ->
                    let (nuse, ndef) = try Temp.Table.find t tab with Not_found -> (0, 0) in
                      Temp.Table.add t (nuse, ndef+1) tab)
                 tab'
                 (Flow.Graph.Table.find fnode def))
          Temp.Table.empty
          flownodes in
        (fun inode ->
           let t = gtemp inode in
           let (use, def) = Temp.Table.find t usedefs in
             if List.mem t rewrite_temps then max_int else use + def) in
    let (allocation, spilled) = Color.color igraph spill_cost Frame.temp_map Frame.registers in
      if spilled = [] then
        (instrs, allocation)
      else
        let (temps, instrs') = rewrite_program spilled frame instrs in
          loop instrs' temps in
    loop instrs []
