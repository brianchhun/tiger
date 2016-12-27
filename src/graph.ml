type node' = int

type noderep = {succ: node' list; pred: node' list}

let empty_node = {succ=[]; pred=[]}

type graph = noderep DynArray.t

type node = graph * node'

let eq ((_, a):node) ((_, b):node) = a = b

let augment g n = (g, n)

let new_graph () = DynArray.create ()

let nodes (g: graph) =
  let len = DynArray.length g in
  let rec nodes' i = if i = len then [] else (g, i) :: nodes' (i +1) in
    nodes' 0

let succ (g, i) =
  let {succ; _ } = DynArray.get g i in
    List.map (augment g) succ

let pred (g, i) =
  let {pred; _} = DynArray.get g i in
    List.map (augment g) pred

let adj gi = pred gi @ succ gi

let new_node g =
  let i = DynArray.length g in
    DynArray.insert g i empty_node; (g, i)

let diddle_edge change (g, i) (g', j) =
  let {succ = si; pred = pi} = DynArray.get g i in
  let {succ = sj; pred = pj} = DynArray.get g j in
    DynArray.set g i {succ = change j si; pred = pi};
    DynArray.set g j {succ = sj; pred = change i pj};
    ()

exception GraphEdge
let rec delete i = function
    [] -> raise GraphEdge
  | j :: tl -> if i = j then tl else j :: delete i tl

let mk_edge = diddle_edge (fun h t -> h :: t)
let rm_edge = diddle_edge delete

let nodename (g, i) = "n" ^ string_of_int i

module Table = Map.Make(
  struct
    type t = node
    let compare (_, i) (_, j) = compare i j
  end)
