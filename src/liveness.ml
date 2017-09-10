open ExtList
open ExtString

type igraph =
  {graph: Igraph.graph;
   tnode: Temp.temp -> Igraph.node;
   gtemp: Igraph.node -> Temp.temp;
   moves: (Igraph.node * Igraph.node) list}

type liveset = unit Temp.Table.t * Temp.temp list
type livemap = liveset Flow.Graph.Table.t

(* Set functions on lists *)

let rec add e = function
    [] -> [e]
  | hd :: tl ->
      if e = hd then
        hd :: tl
      else if e < hd then
        e :: hd :: tl
      else
        hd :: add e tl

let rec union x y =
  match x, y with
    [], _ -> y
  | _, [] -> x
  | hd :: tl, _ ->
      add hd (union tl y)

let rec diff x y =
  match x, y with
    [], _ -> []
  |  _, [] -> x
  | hd :: tl, _ ->
      if List.mem hd y then
        diff tl y
      else
        add hd (diff tl y)

let show_livemap out_channel livemap =
  Flow.Graph.Table.iter
    (fun fnode (_, temps) ->
       output_string out_channel (Flow.Graph.nodename fnode ^ "\t");
       output_string out_channel (String.join " " (List.map (Frame.string_of_temp) temps));
       output_string out_channel "\n")
    livemap

let liveness ({Flow.def; use; ismove; control} as flowgraph) =
  let fnodes = Flow.Graph.nodes control in
  let emptytab = List.fold_left
      (fun tab fnode -> Flow.Graph.Table.add fnode [] tab)
      Flow.Graph.Table.empty
      fnodes in
  let rec loop intab outtab =
    let (intab', outtab') = List.fold_right
        (fun fnode (intab, outtab) ->
           let use = Flow.Graph.Table.find fnode use in
           let def = Flow.Graph.Table.find fnode def in
           let liveout = Flow.Graph.Table.find fnode outtab in
           let livein' = union use (diff liveout def) in
           let liveout' = List.fold_left
               (fun liveout succ ->
                  union liveout (Flow.Graph.Table.find succ intab))
               liveout
               (Flow.Graph.succ fnode) in
           let intab' = Flow.Graph.Table.add fnode livein' intab in
           let outtab' = Flow.Graph.Table.add fnode liveout' outtab in
             (intab', outtab'))
        fnodes
        (intab, outtab) in
      if List.for_all
           (fun fnode ->
             Flow.Graph.Table.find fnode intab = Flow.Graph.Table.find fnode intab' &&
             Flow.Graph.Table.find fnode outtab = Flow.Graph.Table.find fnode outtab')
           fnodes then
        (intab', outtab')
      else
         loop intab' outtab' in
  let (intab, outtab) = loop emptytab emptytab in
    List.fold_left
      (fun livemap fnode ->
         let livelist = Flow.Graph.Table.find fnode outtab in 
         let liveset = List.fold_left
             (fun tab t -> Temp.Table.add t () tab)
             Temp.Table.empty
             livelist in
           Flow.Graph.Table.add fnode (liveset, livelist) livemap)
      Flow.Graph.Table.empty
      fnodes

let interference_graph ({Flow.def; use; ismove; control} as flowgraph) =
  let livemap = liveness flowgraph in
  let igraph = Igraph.new_graph () in
  let add_inode (tnode, gtemp) t =
    if Temp.Table.mem t tnode then
      (tnode, gtemp)
    else
      let inode = Igraph.new_node igraph in
      let tnode' = Temp.Table.add t inode tnode in
      let gtemp' = Igraph.Table.add inode t gtemp in
        (tnode', gtemp') in
  let add_iedge n1 n2 =
    if not (Igraph.eq n1 n2) && not (List.exists (Igraph.eq n1) (Igraph.adj n2)) then
      Igraph.mk_edge n1 n2 in
  let rec loop tnode gtemp moves = function
      [] ->
        {graph=igraph;
         tnode=(fun t -> Temp.Table.find t tnode);
         gtemp=(fun inode -> Igraph.Table.find inode gtemp);
         moves=List.rev moves}
    | fnode :: fnodes ->
        let use = Flow.Graph.Table.find fnode use in
        let def = Flow.Graph.Table.find fnode def in
        let ismove = Flow.Graph.Table.find fnode ismove in
        let (livetab, livelist) = Flow.Graph.Table.find fnode livemap in
        let (tnode', gtemp') = List.fold_left add_inode (tnode, gtemp) (use @ def @ livelist) in
        let moves' = if ismove then
            let u = Temp.Table.find (List.hd def) tnode' in
            let v = Temp.Table.find (List.hd use) tnode' in
              (u, v) :: moves
          else
            moves in
        let adj = if ismove && Temp.Table.mem (List.hd use) livetab then
            List.filter (fun t -> t <> List.hd use) livelist
          else
            livelist in
          List.iter
            (fun d ->
               let inode = Temp.Table.find d tnode' in
               let iadj = List.map (fun t -> Temp.Table.find t tnode') adj in
                 List.iter (add_iedge inode) iadj)
            def;
          loop tnode' gtemp' moves' fnodes in
  let fnodes = Flow.Graph.nodes control in
  let igraph = loop Temp.Table.empty Igraph.Table.empty [] fnodes in
  let liveout fnode = snd (Flow.Graph.Table.find fnode livemap) in
    (igraph, liveout)

let show out_channel {graph; gtemp; _} =
  let say = output_string out_channel in
    say "\n";
  List.iter
    (fun n ->
      say ((Frame.string_of_temp (gtemp n)) ^ ":\t" ^
           (String.join ", " (List.map (fun gn -> Frame.string_of_temp (gtemp gn)) (Igraph.adj n))) ^
           ": " ^
           (string_of_int (List.length (Igraph.adj n))) ^
           "\n"))
    (Igraph.nodes graph)
