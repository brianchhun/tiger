type allocation = Frame.register Temp.Table.t

type 'a dll_node = 'a Dllist.node_t option
type 'a dll = 'a Dllist.node_t option ref

(* Node rep *)
type cnode =
  {inode: Igraph.node;
   n: int;
   mutable setnode: cnode dll_node;
   mutable dll: cnode dll}

module Cset = Set.Make(struct type t = cnode let compare = compare end)

module Rset = Set.Make(struct type t = Frame.register let compare = compare end)

let print_dll dll =
  match !dll with
    None -> print_endline "empty"
  | Some dllist ->
      Dllist.iter
        (fun n -> print_string ((string_of_int n.n) ^ " "))
        dllist;
      print_newline ()

let color ({Liveness.graph; tnode; gtemp; moves} as igraph) spill_cost allocation registers =
  (* Nodeset functions *)
  let create_dll () = ref None in

  let dll2list dll = Option.map_default Dllist.to_list [] !dll in

  let nextn = ref 0 in
  let add_node inode dll =
    let cnode = {inode; n=(!nextn); setnode=None; dll} in
      nextn := !nextn + 1;
      let dllnode = Dllist.create cnode in
        cnode.setnode <- Some dllnode;
        begin
          match !dll with
            None ->
              dll := Some dllnode
          | Some other ->
              Dllist.splice (Dllist.prev other) dllnode
        end;
        cnode in

  let move_node cnode dll =
    let olddll = cnode.dll in
    let head = Option.get !olddll in
    let setnode = Option.get cnode.setnode in
    let next = Dllist.drop setnode in
      cnode.dll <- dll;
      (* Remove from old dll *)
      if next == setnode then
        olddll := None
      else if setnode == head then
        olddll := Some next;
      (* Append to new dll *)
      match !dll with
        None ->
          dll := Some setnode
      | Some other ->
          Dllist.splice (Dllist.prev other) setnode in

  (* Start *)
  let n = List.length (Igraph.nodes graph) in
  let k = List.length registers in

  (* Node sets *)
  let precolored = create_dll () in
  let initial = create_dll () in
  let simplify_worklist = create_dll () in
  let freeze_worklist = create_dll () in
  let spill_worklist = create_dll () in
  let spilled_nodes = create_dll () in
  let coalesced_nodes = create_dll () in
  let colored_nodes = create_dll () in
  let select_stack = create_dll () in

  (* Other data structures *)
  let adj_set = Array.make_matrix n n false in
  let adj_list = Array.make n Cset.empty in
  let degree = Array.make n 0 in
  let color = Array.make n None in

  let add_edge u v =
    if not adj_set.(u.n).(v.n) && u != v then
      begin
        adj_set.(u.n).(v.n) <- true;
        adj_set.(v.n).(u.n) <- true;
        if u.dll != precolored then
          begin
            adj_list.(u.n) <- Cset.add v adj_list.(u.n);
            degree.(u.n) <- degree.(u.n) + 1
          end;
        if v.dll != precolored then
          begin
            adj_list.(v.n) <- Cset.add u adj_list.(v.n);
            degree.(v.n) <- degree.(v.n) + 1
          end
      end in

  let build () =
    let inodetab = List.fold_left
        (fun tab inode ->
           let temp = gtemp inode in
           let cnode = if Temp.Table.mem temp allocation then
               let cnode = add_node inode precolored in
                 color.(cnode.n) <- Some (Temp.Table.find temp allocation);
                 cnode
             else
               add_node inode initial in
             Igraph.Table.add inode cnode tab)
        Igraph.Table.empty 
        (Igraph.nodes graph) in
      List.iter
        (fun inode ->
           let u = Igraph.Table.find inode inodetab in
             List.iter
               (fun adj -> add_edge u (Igraph.Table.find adj inodetab))
               (Igraph.adj inode))
        (Igraph.nodes graph) in

  let make_worklist () =
    List.iter
      (fun cnode ->
         if degree.(cnode.n) >= k then
           move_node cnode spill_worklist
         else
           move_node cnode simplify_worklist)
      (dll2list initial) in

  let adjacent cnode =
    Cset.elements
      (Cset.diff adj_list.(cnode.n)
         (Cset.of_list (dll2list select_stack))) in

  let decrement_degree cnode =
    let d = degree.(cnode.n) in
      degree.(cnode.n) <- degree.(cnode.n) - 1;
      if d = k then
        move_node cnode simplify_worklist in

  let simplify () =
    List.iter
      (fun cnode ->
         move_node cnode select_stack;
         List.iter decrement_degree (adjacent cnode))
      (dll2list simplify_worklist) in

  let assign_colors () =
    List.iter
      (fun cnode ->
         let ok_colors' =
           Cset.fold
             (fun adj ok_colors ->
                if adj.dll == colored_nodes ||
                   adj.dll == precolored then
                  Rset.remove (Option.get color.(adj.n)) ok_colors
                else
                  ok_colors)
             adj_list.(cnode.n) 
             (Rset.of_list registers) in
           if Rset.is_empty ok_colors' then
             move_node cnode spilled_nodes
           else
             begin
               move_node cnode colored_nodes;
               color.(cnode.n) <- Some (Rset.choose ok_colors')
             end)
      (List.rev (dll2list select_stack)) in

  let select_spill () =
    let cost cnode = spill_cost cnode.inode in
    let m :: _ = List.sort (fun u v -> cost u - cost v) (dll2list spill_worklist) in
      move_node m simplify_worklist in

    build ();
    make_worklist ();

    while (dll2list simplify_worklist) <> [] ||
          (dll2list spill_worklist) <> [] do
      if (dll2list simplify_worklist) <> [] then simplify ()
      else if (dll2list spill_worklist) <> [] then select_spill ()
    done;

    assign_colors ();

    let allocation' =
      List.fold_left
        (fun alloc cnode ->
           let c = Option.get color.(cnode.n) in
           let t = gtemp cnode.inode in
             Temp.Table.add t c alloc)
        allocation
        (dll2list colored_nodes) in
    let spilled = List.map (fun cnode -> gtemp cnode.inode) (dll2list spilled_nodes) in
      (allocation', spilled)
