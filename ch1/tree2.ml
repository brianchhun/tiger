(* 1.1c *)
type key = string

type 'a binding = key * 'a

type 'a tree = LEAF | TREE of 'a tree * 'a binding * 'a tree

let empty = LEAF

let rec insert tree key value =
  match tree with
  | LEAF -> TREE(empty,(key,value),empty)
  | TREE (l,(k,v),r) ->
     if key < k then TREE(insert l key value,(k,v),r)
     else if key > k then TREE(l,(k,v),insert r key value)
     else TREE(l,(key,value),r)

let rec lookup tree key =
  match tree with
  | LEAF -> None
  | TREE(l,(k,v),r) ->
     if key < k then lookup l key
     else if key > k then lookup r key
     else Some v
