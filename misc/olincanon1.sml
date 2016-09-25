(* Canonicalise, simple version
 ******************************************************************************
 * Olin Shivers, 2002/10
 *
 * Appel's canon.sml module is clever to the point of being incomprehensible.
 * It relies on obscure invariants about patterns in the tree that are never
 * spelled out. It's quite hard to really understand.
 *
 * Here is a version that is much easier to understand.
 * - Invariants are encoded in the type system.
 *   There is an actual AST defined that spells out what we are doing:
 *   we go from a Tree language to a Tree' language. Anytime you see
 *   a constructor or a var name with a prime, it's in the Tree' language.
 *
 *   Encoding the Tree' restrictions in the type declaration mean that you
 *   know *for sure* the canonicaliser has respected them -- it *can't*
 *   construct an expression that violates them. The type structure will lead
 *   you through the task of building a "canonicalised" Tree' program -- just
 *     1. Handle all the datatype cases for the input Tree forms, and
 *     2. Construct values in the result Tree' datatype
 *   and you've got a transformer that guaranteed goes from Tree programs to
 *   Tree' programs. Period, end of story -- classic SML religion.
 * 
 * - Clearer algorithmic logic
 *   Appel's treatment of CALL & MOVE nodes are the trickiest,
 *   hardest-to-understand parts of his code, and the special cases to make
 *   this work out pop up in very unintuitive places. My code handles these
 *   things in a straightforward, unexceptional way. 
 *
 *   In particular, the interesting thing this code does is have *three*
 *   canonicalisers: one for expressions, statements, and "rvals," which
 *   are "right-hand-side values" -- expressions used as sources in MOVE
 *   statements. Andrew only has canonicalisers for the first two cases,
 *   and has special cases inserted here & there through the code to make
 *   things handled by my rval canonicaliser (CALL & MOVE nodes) work out. 
 *   Much subtler code.
 *   
 *   Note, however, the way I handle expression-internal calls requires a
 *   smarter COMMUTE function than Andrew needs. It generates better code (no
 *   gratuitous temp->temp moves) and (more importantly) simplifies the
 *   canonicaliser logic so that it is much more straightforward to
 *   understand. Recall that a call subexpression gets converted to a
 *   (t:=call; t) stmt/expression pair, for a fresh temp t. The "t:=call"
 *   assignment then bubbles up out of the exp tree, leaving behind the pure
 *   t. My code needs to realise that the fresh temp thus introduced will
 *   commute with any statement (since no other statement writes that temp).
 *   But this isn't *tricky* -- it's just code.
 *
 * - The code is commented. This, I find, frequently helps code clarity.
 *
 * - I made at least one sacrifice to efficiency.
 *   Canonicalising a Tree.stm produces a *list* of simple Tree' statements
 *   (that is, a stm' list). All through my code, I recursively construct
 *   these lists, then paste them together with append (@). Using append in
 *   a recursion is asking for O(n^2) space & time, because append copies
 *   its entire first argument. Andrew is smart about this -- he just *cons* 
 *   the two results together with a SEQ, then when he's all done, he
 *   has a little function linearize that walks this SEQ tree -- right child
 *   first, then left child -- consing leaf items onto a list. So, linear
 *   in time & space.
 *
 *   This is better, but much more confusing, since it means the
 *   very functions that are supposed to be *getting rid* of SEQs are
 *   *introducing* them. It turns out those SEQs always bubble up to the top
 *   of the SEQ tree... but that's just one more property you have to keep
 *   in mind. *My* code produces a stmt' list, so it's much, much clearer 
 *   what's going on.
 *
 *   Andrew also introduces noops that are always removed by the % function.
 *   I didn't have to mess with that. A noop is an empty list of statements.
 * 
 * - I punted Andrew's reorder_exp & reorder_stm, the functions that take
 *   "builder" context-insertion lambdas. They buy very little (they allow you
 *   to abstract away a simple three-line idiom to a complicated one-line
 *   function call) but add a layer of confusing complexity for the reader of
 *   the code -- who, at Georgia Tech, is typically a top student, but one
 *   who has nonetheless been hacking SML for a grand total of two months
 *   (those months being September & October).
 *
 * You could take my code, replace the Tree' language with a Tree sublanguage,
 * replace the statement lists with statement trees, do a post-pass
 * linearisation of the result tree, and you'd have a drop-in replacement for
 * Andrew's code. I have done this in another file. This file is purely for
 * explanatory reasons. Understand *it*, and you will completely understand the
 * canonicalisation process; then you can tackle my plug-in-replacement 
 * version easily.
 *)

structure OlinCanon = struct
local
  open Tree
in

(* The Tree' language: stm' rval' and exp'
 ******************************************************************************
 * This is the Tree language, with some strong restrictions.
 * 1. Eliminate SEQ & EXP from the statement language.
 * 2. Allow calls only as the right-hand source of a MOVE stmt or
 *    as a top-level call that doesn't produce a value. That is, calls come
 *    up to top-level -- they are not part of the exp' language, so can't
 *    appear way down inside some expression.
 * A list of these things looks much more like "generic" assembler:
 *    FACT:				// LABEL'
 *      if n=0 then L1 else L2		// CJUMP'
 *    L1:				// LABEL'
 *      rv := 1				// MOVE'
 *      goto DONE			// JUMP'
 *    L2:				// LABEL'
 *      r := FACT(n-1)			// MOVE'
 *      a := n * r			// MOVE'
 *      rv := a				// MOVE'
 *      goto DONE			// JUMP'
 * The right-hand sides of register assignments & memory stores can be pure
 * expression trees or function calls. Funcall args must be pure expression
 * trees.
 * 
 *)
datatype stm' = LABEL' of label
              | JUMP' of exp' * label list
              | CJUMP' of relop * exp' * exp' * label * label
	      | MOVE' of exp' * rval         (* Target is MEM' or TEMP' node. *)
              | TOPCALL' of exp' * exp' list

(* Right-hand-side value for MOVE statements: exp' or a function call. *)
and rval = RVEXP of exp' | RVCALL of exp' * exp' list

(* No ESEQ or CALL. Pure: side-effect-free, guaranteed to terminate. *)
and exp' = BINOP' of binop * exp' * exp'
         | MEM' of exp'
	 | TEMP' of Temp.temp
	 | NAME' of label
	 | CONST' of int

(* commutes(stmts', exp') -> bool
 ******************************************************************************
 * - Exp' is a side-effect-free Tree' expression -- won't write a temp or 
 *   memory, guaranteed to terminate. 
 * - Stmts' is a list of Tree' statements.
 *   
 * - Return true if we are sure that (1) executing (stmts'; exp') produces
 *   the same value as (t := exp'; stmts; t) for some fresh variable t.
 * - Return false if the two executions would differ... or if we're not sure.
 *   
 * A legal defn of this function: fn _ => false.
 *
 * Control effects: Note that if stmts' contains a jump off to somewhere else,
 * neither expression produces any value at all! So they would commute. On
 * the other hand, if stmts' contains a jump outside of stmts', where we might
 * execute arbitrary code, *then jump back*, said arbitrary code might write
 * a location referenced by exp'... so we throw up our hands and safely say
 * false. You can't write a perfect commutation analysis, so don't lose sleep
 * over it. This one is good enough to handle the way we yank calls up to top
 * level, which is a desireable property.
 *)
fun commutes(_, NAME' _)       = true
  | commutes(_, CONST' _)      = true
  | commutes(stmts, BINOP'(_,e1,e2)) = commutes(stmts, e1) andalso commutes(stmts,e2)
  | commutes(stmts, TEMP' t) =
      List.all (fn (LABEL' _)   => true
		 | (JUMP' _)    => false	(* Maybe *)
		 | (CJUMP' _)   => false	(* Maybe *)
		 | (TOPCALL' _) => true		(* Temps per-procedure call. *)
		 | (MOVE'(TEMP' t',_)) => not (t = t')
		 | (MOVE' _)          => true)
          stmts
  | commutes(stmts, MEM' _) =
      List.all (fn (MOVE'(MEM' _,_))   => false (* Maybe *)
		 | (JUMP' _)           => false (* Maybe *)
		 | (CJUMP' _)          => false (* Maybe *)
		 | (TOPCALL' _)        => false (* Maybe *)
		 | (MOVE'(_,RVCALL _)) => false (* Maybe *)
		 | _                   => true)
	       stmts


(* do_exps: exp list -> (stm' list * exp' list)
 ******************************************************************************
 * Purify a list of N expressions -- boil out the side-effects into a 
 * stmt' list, and render the left-over, pure expressions as an N-elt
 * exp' list.
 *)
fun do_exps (exp::exps) =
    let val (stms1',exp')  = do_exp  exp
	val (stms2',exps') = do_exps exps
    in if commutes(stms2',exp')
       then (stms1' @ stms2', exp'::exps')
       else let val t = TEMP'(Temp.newtemp())
	    in ( stms1' @ [MOVE'(t, RVEXP exp')] @ stms2'  ,  t::exps' )
	    end
    end
  | do_exps [] = ([], [])



(* do_stmt: stm -> stm' list
 ******************************************************************************
 * Turn a compex stm statement into a list of simple stm' statements.
 *)
and do_stm(SEQ(a,b)) =         do_stm a @ do_stm b
  | do_stm(JUMP(e,labs)) =     let val (stms',e') = do_exp e
		 	       in stms' @ [JUMP'(e',labs)]
			       end
  | do_stm(CJUMP(p,a,b,t,f)) = let val (stms',[a',b']) = do_exps [a,b]
			       in stms' @ [CJUMP'(p,a',b',t,f)]
			       end
  | do_stm(MOVE(TEMP t,b)) =   let val (stms', rv) = do_rval b
			       in stms' @ [MOVE'(TEMP' t, rv)]
			       end
  | do_stm(MOVE(MEM e,b)) =    let val (stms1', e')  = do_exp e
				   val (stms2', rv)  = do_rval b
			       in if commutes(stms2', e')
				  then stms1' @ stms2' @ [MOVE'(MEM' e', rv)]
				  else let val t = TEMP'(Temp.newtemp())
				       in stms1' @ [MOVE'(t, RVEXP e')]
					         @ stms2'
						 @ [MOVE'(MEM' t, rv)]
				       end
			       end
  | do_stm(EXP e) = 	       let val (stms', _) = do_exp e
		    	       in stms'  (* Cool, eh? *)
		    	       end
  | do_stm(LABEL l) =          [LABEL' l]


(* do_exp : exp -> stm' list * exp'
 ******************************************************************************
 * Purify a complex expression. Boil out the side-effects into a stm' list,
 * and render the left-over, pure expression as an exp'.
 *)
and do_exp(BINOP(p,a,b)) = let val (stms',[a',b']) = do_exps [a,b]
			   in (stms',BINOP'(p,a',b'))
			   end
 			   
  | do_exp(MEM a) = 	   let val (stms',a') = do_exp a
		    	   in (stms', MEM' a')
		    	   end
 			   
  | do_exp(ESEQ(s,e)) =    let val stms1' = do_stm s
			       val (stms2',e') = do_exp e
			   in (stms1' @ stms2', e')
			   end

  | do_exp(CALL(f,args)) = let val t = TEMP'(Temp.newtemp())
			       val (stmts', f'::args') = do_exps(f::args)
			   in (stmts' @ [MOVE'(t, RVCALL(f',args'))]  ,  t)
			   end

  | do_exp(TEMP  t) = 	   ([], TEMP' t)
  | do_exp(CONST i) = 	   ([], CONST' i)
  | do_exp(NAME  l) = 	   ([], NAME' l)

(* do_rval : exp -> stm' list * rval
 ******************************************************************************
 * EXP is an expression that is the source of a MOVE statement. Boil out
 * all of its side effects into a stm' list, and render the result as
 * a suitable rval.
 *)
and do_rval(ESEQ(s,e)) =    let val stms1' = do_stm s
			    	val (stms2',r') = do_rval e
			    in (stms1' @ stms2', r')
			    end

  | do_rval(CALL(f,args)) = let val (stmts,f'::args') = do_exps(f::args)
			    in (stmts, RVCALL(f', args'))
			    end

  | do_rval exp  = 	    let val (stmts', exp') = do_exp exp
		   	    in (stmts', RVEXP exp')
		   	    end
end (*local*)
end (*struct*)
