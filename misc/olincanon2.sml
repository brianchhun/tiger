(* A drop-in-replacement version for Appel's Tree canonicaliser.
 ******************************************************************************
 * Olin Shivers, 2002/10, with bugfix by Ivan Raikov.
 *
 * Appel's canon.sml module is clever to the point of being incomprehensible.
 * It relies on obscure invariants about patterns in the tree that are never
 * spelled out. It's quite hard to really understand.
 *
 * Here is a version that is much easier to understand.
 *
 * There are no comments in this code, because it is simply a transcription 
 * of the version in olincanon1.sml, with the following differences:
 * - Rather than define the new Tree' language as a separate datatype,
 *   we just translate to a sublanguage of Tree. 
 * - We represent a sequence of canonicalised statements as a binary tree
 *   constructed from SEQ's and leaf nodes. This allows us to "append" two
 *   "sequences" in constant time. We subsequently linearise the tree into
 *   a SEQ-free list, again in linear time. A nop statement is used to
 *   represent the empty sequence.
 * See olincanon1.sml for detailed comments.
 *
 * I've only rewritten the canonicaliser, linearize. I didn't rewrite the
 * basicBlocks or traceSchedule functions. Have at it.
 *   -Olin
 *)

signature CANON = sig
    val linearize : Tree.stm -> Tree.stm list
        (* From an arbitrary Tree statement, produce a list of cleaned trees
	   satisfying the following properties:
	      1.  No SEQ's or ESEQ's
	      2.  The parent of every CALL is an EXP(..) or a MOVE(TEMP t,..)
        *)

    val basicBlocks : Tree.stm list -> (Tree.stm list list * Tree.label)
        (* From a list of cleaned trees, produce a list of
	 basic blocks satisfying the following properties:
	      1. and 2. as above;
	      3.  Every block begins with a LABEL;
              4.  A LABEL appears only at the beginning of a block;
              5.  Any JUMP or CJUMP is the last stm in a block;
              6.  Every block ends with a JUMP or CJUMP;
           Also produce the "label" to which control will be passed
           upon exit.
        *)

    val traceSchedule : Tree.stm list list * Tree.label -> Tree.stm list
         (* From a list of basic blocks satisfying properties 1-6,
            along with an "exit" label,
	    produce a list of stms such that:
	      1. and 2. as above;
              7. Every CJUMP(_,t,f) is immediately followed by LABEL f.
            The blocks are reordered to satisfy property 7; also
	    in this reordering as many JUMP(T.NAME(lab)) statements
            as possible are eliminated by falling through into T.LABEL(lab).
         *)
end

structure Canon : CANON = struct
local
  open Tree
in

infix %				(* Paste together two stmts       *)
fun (EXP(CONST _)) % x = x	(* (but drop noops on the floor). *)
  | x % (EXP(CONST _)) = x
  | x % y = SEQ(x,y)

val nop = EXP(CONST 0)

(* Apply f:stm->bool to every leaf node in a stmt tree;
 * "and" the results together.
 *)
fun stmtall f stm = let fun recur(SEQ(a,b)) = recur a andalso recur b
			  | recur s         = f s
		    in recur stm
		    end


fun commutes(_, NAME _)       = true
  | commutes(_, CONST _)      = true
  | commutes(stmt, BINOP(_,e1,e2)) = commutes(stmt,e1) andalso commutes(stmt,e2)
  | commutes(stmt, TEMP t) =
      stmtall (fn (LABEL _)   => true
		| (JUMP _)    => false	(* Maybe *)
		| (CJUMP _)   => false	(* Maybe *)
		| (EXP(CALL _)) => true	(* Temps per-procedure call. *)
		| (MOVE(TEMP t',_)) => not (t = t')
		| (MOVE _)          => true
		| (EXP _) => true)	(* I think this case will only be     *)
          stmt				(* triggered by noop's -- EXP(CONST _)*)
  | commutes(stmt, MEM _) =
      stmtall (fn (MOVE(MEM _,_))    => false (* Maybe *)
		| (JUMP _)           => false (* Maybe *)
		| (CJUMP _)          => false (* Maybe *)
		| (EXP(CALL _))      => false (* Maybe *)
		| (MOVE(_,CALL _))   => false (* Maybe *)
		| _                  => true)
	      stmt


fun do_exps (exp::exps) =
    let val (stm1',exp') = do_exp exp
	val (stm2',exps') = do_exps exps
    in if commutes(stm2',exp')
       then (stm1' % stm2', exp'::exps')
       else let val t = TEMP(Temp.newtemp())
	    in ( stm1' % MOVE(t, exp') % stm2'  ,  t::exps' )
	    end
    end
  | do_exps [] = (nop, [])	(* Noop *)


and do_stm(SEQ(a,b)) =         do_stm a % do_stm b
  | do_stm(JUMP(e,labs)) =     let val (stm',e') = do_exp e
		 	       in stm' % JUMP(e',labs)
			       end
  | do_stm(CJUMP(p,a,b,t,f)) = let val (stm',[a',b']) = do_exps [a,b]
			       in stm' % CJUMP(p,a',b',t,f)
			       end
  | do_stm(MOVE(TEMP t,b)) =   let val (stm', rv) = do_rval b
			       in stm' % MOVE(TEMP t, rv)
			       end
  | do_stm(MOVE(MEM e,b)) =    let val (stm1', e')  = do_exp e
				   val (stm2', rv)  = do_rval b
			       in if commutes(stm2', e')
				  then stm1' % stm2' % MOVE(MEM e', rv)
				  else let val t = TEMP(Temp.newtemp())
				       in stm1' % MOVE(t, e')
					        % stm2'
						% MOVE(MEM t, rv)
				       end
			       end
  | do_stm(EXP e) = 	       let val (stm', _) = do_exp e
		    	       in stm'
		    	       end
  | do_stm label =	       label


and do_exp(BINOP(p,a,b)) = let val (stm',[a',b']) = do_exps [a,b]
			   in (stm',BINOP(p,a',b'))
			   end
 			   
  | do_exp(MEM a) = 	   let val (stm',a') = do_exp a
		    	   in (stm', MEM a')
		    	   end
 			   
  | do_exp(ESEQ(s,e)) =    let val stm1' = do_stm s
			       val (stm2',e') = do_exp e
			   in (stm1' % stm2', e')
			   end

  | do_exp(CALL(f,args)) = let val t = TEMP(Temp.newtemp())
			       val (stm', f'::args') = do_exps(f::args)
			   in (stm' % MOVE(t, CALL(f',args'))  ,  t)
			   end

  | do_exp other =         (nop, other)


and do_rval(ESEQ(s,e)) =    let val stm1' = do_stm s
			    	val (stm2',r') = do_rval e
			    in (stm1' % stm2', r')
			    end

  | do_rval(CALL(f,args)) = let val (stm',f'::args') = do_exps(f::args)
			    in (stm', CALL(f', args'))
			    end

  | do_rval exp  = 	    let val (stm', exp') = do_exp exp
		   	    in (stm', exp')
		   	    end

fun linear(SEQ(a,b),l) = linear(a,linear(b,l)) (* Flatten top-level SEQ's    *)
  | linear(s,l) = s::l			       (* into a list of leaf stmts. *)

fun linearize stm0 = linear(do_stm stm0, nil)

(* From here down it's just Andrew's lineariser code
 ******************************************************************************
 *)

  structure T = Tree

  type block = T.stm list

  (* Take list of statements and make basic blocks satisfying conditions
       3 and 4 above, in addition to the extra condition that 
      every block ends with a JUMP or CJUMP *)

  fun basicBlocks stms = 
     let val done = Temp.newlabel()
         fun blocks((head as T.LABEL _) :: tail, blist) =
	     let fun next((s as (T.JUMP _))::rest, thisblock) =
		                endblock(rest, s::thisblock)
		   | next((s as (T.CJUMP _))::rest, thisblock) =
                                endblock(rest,s::thisblock)
		   | next(stms as (T.LABEL lab :: _), thisblock) =
                                next(T.JUMP(T.NAME lab,[lab]) :: stms, thisblock)
		   | next(s::rest, thisblock) = next(rest, s::thisblock)
		   | next(nil, thisblock) = 
			     next([T.JUMP(T.NAME done, [done])], thisblock)
		 
		 and endblock(stms, thisblock) = 
		            blocks(stms, rev thisblock :: blist)
		     
	     in next(tail, [head])
	     end
	   | blocks(nil, blist) = rev blist
	   | blocks(stms, blist) = blocks(T.LABEL(Temp.newlabel())::stms, blist)
      in (blocks(stms,nil), done)
     end

  fun enterblock(b as (T.LABEL s :: _), table) = Symbol.enter(table,s,b)
    | enterblock(_, table) = table

  fun splitlast([x]) = (nil,x)
    | splitlast(h::t) = let val (t',last) = splitlast t in (h::t', last) end

  fun trace(table,b as (T.LABEL lab :: _),rest) = 
   let val table = Symbol.enter(table, lab, nil)
    in case splitlast b
     of (most,T.JUMP(T.NAME lab, _)) =>
	  (case Symbol.look(table, lab)
            of SOME(b' as _::_) => most @ trace(table, b', rest)
	     | _ => b @ getnext(table,rest))
      | (most,T.CJUMP(opr,x,y,t,f)) =>
          (case (Symbol.look(table,t), Symbol.look(table,f))
            of (_, SOME(b' as _::_)) => b @ trace(table, b', rest)
             | (SOME(b' as _::_), _) => 
		           most @ [T.CJUMP(T.notRel opr,x,y,f,t)]
		                @ trace(table, b', rest)
             | _ => let val f' = Temp.newlabel()
		     in most @ [T.CJUMP(opr,x,y,t,f'), 
				T.LABEL f', T.JUMP(T.NAME f,[f])]
			     @ getnext(table,rest)
                        end)
      | (most, T.JUMP _) => b @ getnext(table,rest)
     end

  and getnext(table,(b as (T.LABEL lab::_))::rest) = 
           (case Symbol.look(table, lab)
             of SOME(_::_) => trace(table,b,rest)
              | _ => getnext(table,rest))
    | getnext(table,nil) = nil

  fun traceSchedule(blocks,done) = 
       getnext(foldr enterblock Symbol.empty blocks, blocks)
         @ [T.LABEL done]

end (*local*)
end (*struct*)
