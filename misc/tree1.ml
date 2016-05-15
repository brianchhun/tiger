type key = string

type tree = LEAF | TREE of tree * key * tree

let empty = LEAF

let rec insert key tree =
  match tree with
  | LEAF -> TREE(empty,key,empty)
  | TREE (l,k,r) ->
     if k < key then TREE(insert key l,k,r)
     else if k > key then TREE(l,k,insert key r)
     else TREE(l,key,r)

(* 1.1b *)
let rec member key tree =
  match tree with
  | LEAF -> false
  | TREE (l,k,r) ->
     if k < key then member key l
     else if k > key then member key r
     else true

