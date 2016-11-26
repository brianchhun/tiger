type temp = int
let nexttemp = ref 100
let new_temp () =
  let i = !nexttemp in
    nexttemp := !nexttemp + 1;
    i
let string_of_temp t = "t" ^ string_of_int t

type label = Symbol.t
let nextlabel = ref 0
let new_label () =
  let l = Symbol.symbol (Printf.sprintf "L%d" !nextlabel) in
    nextlabel := !nextlabel + 1;
    l
let named_label = Symbol.symbol
let string_of_label = Symbol.name

module Table = Map.Make(
  struct
    type t = temp
    let compare = compare
  end)
