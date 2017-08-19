(* Node rep *)
type cnode =
  {inode: int; (* Igraph.node; *)
   n: int;
   mutable setnode: cnode Dllist.node_t option;
   mutable nodeset: nodeset}

(* Node set rep *)
and nodeset = cnode Dllist.node_t option ref

let setnode cnode = Option.get cnode.setnode

let nextn = ref 0
let add_node inode nodeset =
  let cnode = {inode; n=(!nextn); setnode=None; nodeset} in
    nextn := !nextn + 1;
    let dllnode = Dllist.create cnode in
      cnode.setnode <- Some dllnode;
      begin
        match !nodeset with
          None ->
            nodeset := Some dllnode
        | Some other ->
            Dllist.splice (Dllist.prev other) dllnode
      end;
      cnode

let move_node cnode nodeset =
  let oldnodeset = cnode.nodeset in
  let head = Option.get !oldnodeset in
  let setnode = Option.get cnode.setnode in
  let next = Dllist.drop setnode in
    (* Remove from old nodeset *)
    if next == setnode then
      oldnodeset := None
    else if setnode == head then
      oldnodeset := Some next;
    (* Append to new nodeset *)
    match !nodeset with
      None ->
        nodeset := Some setnode
    | Some other ->
        Dllist.splice (Dllist.prev other) setnode

let print_nodeset nodeset =
  match !nodeset with
    None -> print_endline "empty"
  | Some dllist ->
      List.iter
        (fun n -> print_string ((string_of_int n.inode) ^ " "))
        (Dllist.to_list dllist);
      print_newline ()

let create_nodeset () = ref None

let nodeset1 = create_nodeset ();;
let nodeset2 = create_nodeset ();;

let cnode1 = add_node 1 nodeset1;;
let cnode2 = add_node 2 nodeset2;;

(* ns1: 1
 * ns2: 2 *)

print_nodeset nodeset1;;
print_nodeset nodeset2;;

move_node cnode1 nodeset2;;

(* ns1:
 * ns2: 2 1 *)

print_nodeset nodeset1;;
print_nodeset nodeset2;;

let cnode3 = add_node 3 nodeset1;;
let cnode4 = add_node 4 nodeset2;;

(* ns1: 3
 * ns2: 2 1 4 *)

print_nodeset nodeset1;;
print_nodeset nodeset2;;

move_node cnode2 nodeset1;;

(* ns1: 3 2
 * ns2: 1 4 *)

print_nodeset nodeset1;;
print_nodeset nodeset2;;

move_node cnode2 nodeset2;;
move_node cnode4 nodeset1;;

(* ns1: 3 4
 * ns2: 1 2 *)

print_nodeset nodeset1;;
print_nodeset nodeset2;;
