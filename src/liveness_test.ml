open OUnit2

let cmpnodes = List.for_all2 Igraph.eq

let assert_liveset livemap fnode temps =
  let (liveset, livelist) = Flow.Graph.Table.find fnode livemap in
    assert_equal (List.map fst (Temp.Table.bindings liveset)) temps;
    assert_equal livelist temps

let test1 test_ctxt =
  let t1 = Temp.new_temp () in
  let t2 = Temp.new_temp () in
  let t3 = Temp.new_temp () in
  let t4 = Temp.new_temp () in
  let instrs = [
    Assem.OPER {Assem.
                 assem = "lw `d0, 1";
                 src = [];
                 dst = [t1];
                 jump = None};
    Assem.OPER {Assem.
                 assem = "lw `d0, 2";
                 src = [];
                 dst = [t2];
                 jump = None};
    Assem.OPER {Assem.
                 assem = "add 'd0, 's0, 's1";
                 src = [t1; t2];
                 dst = [t3];
                 jump = None};
    Assem.OPER {Assem.
                 assem = "addi 'd0, 's0, 5";
                 src = [t3];
                 dst = [t3];
                 jump = None};
    Assem.OPER {Assem.
                 assem = "jal 's0";
                 src = [t4];
                 dst = [];
                 jump = None};
  ] in
  let (flowgraph, fnodes) = Make_graph.instrs2graph instrs in
  let livemap = Liveness.liveness flowgraph in
    List.iter2
      (assert_liveset livemap)
      fnodes
      [[t1; t4]; [t1; t2; t4]; [t3; t4]; [t4]; []]

let test3 test_ctxt =
  let t1 = Temp.new_temp () in
  let t2 = Temp.new_temp () in
  let t3 = Temp.new_temp () in
  let instrs = [
    Assem.OPER {Assem.
                 assem = "lw `d0, 1";
                 src = [];
                 dst = [t1];
                 jump = None};
    Assem.OPER {Assem.
                 assem = "lw `d0, 2";
                 src = [];
                 dst = [t2];
                 jump = None};
    Assem.OPER {Assem.
                 assem = "add 'd0, 's0, 's1";
                 src = [t1; t2];
                 dst = [t3];
                 jump = None};
    Assem.OPER {Assem.
                 assem = "addi 'd0, 's0, 5";
                 src = [t3];
                 dst = [t3];
                 jump = None}
  ] in
  let (flowgraph, fnodes) = Make_graph.instrs2graph instrs in
  let ({Liveness.graph; tnode; gtemp; moves} as g, liveout) = Liveness.interference_graph flowgraph in
    (* Liveness.show stdout g; *)
    assert_equal 3 (List.length (Igraph.nodes graph));
    List.iter (fun t -> assert_equal (gtemp (tnode t)) t) [t1; t2; t3];
    assert_equal ~cmp:cmpnodes [tnode t2] (Igraph.adj (tnode t1));
    assert_equal ~cmp:cmpnodes [tnode t1] (Igraph.adj (tnode t2));
    assert_equal ~cmp:cmpnodes [] (Igraph.adj (tnode t3))

let test4 test_ctxt =
  let t1 = Temp.new_temp () in
  let t2 = Temp.new_temp () in
  let t3 = Temp.new_temp () in
  let t4 = Temp.new_temp () in
  let instrs = [
    Assem.OPER {Assem.
                 assem = "lw `d0, 1";
                 src = [];
                 dst = [t4];
                 jump = None};
    Assem.OPER {Assem.
                 assem = "lw `d0, 1";
                 src = [];
                 dst = [t1];
                 jump = None};
    Assem.MOVE {Assem.
                 assem = "move 'd0, 's0";
                 src = t1;
                 dst = t2};
    Assem.OPER {Assem.
                 assem = "add 'd0, 's0, 's1";
                 src = [t1; t2];
                 dst = [t3];
                 jump = None};
    Assem.OPER {Assem.
                 assem = "addi 'd0, 's0, 1";
                 src = [t4];
                 dst = [t4];
                 jump = None};
    Assem.MOVE {Assem.
                 assem = "move 'd0, 's0";
                 src = t4;
                 dst = t4};
  ] in
  let (flowgraph, fnodes) = Make_graph.instrs2graph instrs in
  let ({Liveness.graph; tnode; gtemp; moves} as g, liveout) = Liveness.interference_graph flowgraph in
    Liveness.show stdout g;
    assert_equal 4 (List.length (Igraph.nodes graph));
    List.iter (fun t -> assert_equal (gtemp (tnode t)) t) [t1; t2; t3; t4];
    assert_equal ~cmp:cmpnodes [tnode t4] (Igraph.adj (tnode t1));
    assert_equal ~cmp:cmpnodes [tnode t4] (Igraph.adj (tnode t2));
    assert_equal ~cmp:cmpnodes [tnode t4] (Igraph.adj (tnode t3));
    assert_equal ~cmp:cmpnodes (List.map tnode [t3; t2; t1]) (Igraph.adj (tnode t4));
    assert_equal 2 (List.length moves);
    List.iter2 (fun (dst, src) (dst', src') ->
        assert_equal ~cmp:Igraph.eq dst dst';
        assert_equal ~cmp:Igraph.eq src src')
      moves
      [
         (tnode t2, tnode t1);
         (tnode t4, tnode t4)
      ]
        

let suite =
  "suite" >:::
  ["test1">:: test1;
   "test3" >:: test3;
   "test4" >:: test4]

let _ =
  run_test_tt_main suite
