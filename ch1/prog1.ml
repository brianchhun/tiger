type id = string

type binop = Plus | Minus | Times | Div

type
  stm = CompoundStm of stm * stm
      | AssignStm of id * exp
      | PrintStm of exp list
and exp = IdExp of id
	| NumExp of int
	| OpExp of exp * binop * exp
	| EseqExp of stm * exp

type table = id * int list

exception Missing of id

let rec lookup t id = 
  match t with
  | [] -> raise (Missing id)
  | (id1,n)::t2 -> if id=id1 then n else lookup t2 id

let update t id n = (id,n)::t

let rec maxargs = function
  | CompoundStm (stm1,stm2) -> max (maxargs stm1) (maxargs stm2)
  | AssignStm (id,exp) -> maxargsExp exp 
  | PrintStm (l) -> 
     List.fold_left (fun n e -> max n (maxargsExp e)) (List.length l) l
and maxargsExp = function
  | IdExp (id) -> 0
  | NumExp (n) -> 0
  | OpExp (exp1,binop,exp2) -> max (maxargsExp exp1) (maxargsExp exp2)
  | EseqExp (stm,exp) -> max (maxargs stm) (maxargsExp exp)

let applyBinop binop n m =
  match binop with
  | Plus -> n + m
  | Minus -> n - m
  | Times -> n * m
  | Div -> n / m

let rec interpStm stm table =
  match stm with
  | CompoundStm (stm1,stm2) -> interpStm stm2 (interpStm stm1 table)
  | AssignStm (id,exp) ->
     let n, t2 = interpExp exp table in
     update t2 id n
  | PrintStm (l) ->
     List.fold_left (fun t e ->
	 let (n,t2) = interpExp e t in
	 print_int n ;
	 print_newline () ;
	 t2) table l
and interpExp exp table =
  match exp with
  | IdExp (id) -> (lookup table id,table)
  | NumExp (n) -> (n,table)
  | OpExp (exp1,binop,exp2) ->
     let n, t2 = interpExp exp1 table in
     let m, t3 = interpExp exp2 t2 in
     (applyBinop binop n m,t3)
  | EseqExp (stm,exp1) -> interpExp exp1 (interpStm stm table)

let interp stm = ignore (interpStm stm [])

let prog = CompoundStm(
	       AssignStm(
		   "a",
		   OpExp(NumExp 5, Plus, NumExp 3)),
	       CompoundStm(
		   AssignStm(
		       "b",
		       EseqExp(
			   PrintStm[IdExp "a";OpExp(IdExp "a", Minus,NumExp 1)],
			   OpExp(NumExp 10, Times, IdExp"a"))),
		   PrintStm[IdExp "b"]))
