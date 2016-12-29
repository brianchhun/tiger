open OUnit2

let cmpnodes = List.for_all2 Graph.eq

let test1 test_ctxt =
  let t1 = Temp.new_temp () in
  let t2 = Temp.new_temp () in
  let instrs = [
    Assem.OPER {Assem.
                 assem = "sw `s1, -4('s0)";
                 src = [Frame.fp; t1];
                 dst = [];
                 jump = None};
    Assem.LABEL {Assem.
                  assem = "l11";
                  lab = Temp.named_label "l11"};
    Assem.MOVE {Assem.
                 assem = "move 'd0, 's0";
                 src = t1;
                 dst = t2}
  ] in
  let ({Flow.control; def; use; ismove}, [n1; n2; n3]) = Make_graph.instrs2graph instrs in
    assert_equal 3 (List.length (Graph.nodes control));
    (* control graph edges *)
    assert_equal ~cmp:cmpnodes [] (Graph.pred n1);
    assert_equal ~cmp:cmpnodes [n2] (Graph.succ n1);
    assert_equal ~cmp:cmpnodes [n1] (Graph.pred n2);
    assert_equal ~cmp:cmpnodes [n3] (Graph.succ n2);
    assert_equal ~cmp:cmpnodes [] (Graph.succ n3);
    assert_equal ~cmp:cmpnodes [n2] (Graph.pred n3);
    (* def *)
    assert_equal [] (Graph.Table.find n1 def);
    assert_equal [] (Graph.Table.find n2 def);
    assert_equal [t2] (Graph.Table.find n3 def);
    (* use *)
    assert_equal [Frame.fp; t1] (Graph.Table.find n1 use);
    assert_equal [] (Graph.Table.find n2 use);
    assert_equal [t1] (Graph.Table.find n3 use);
    (* ismove *)
    assert_equal false (Graph.Table.find n1 ismove);
    assert_equal false (Graph.Table.find n2 ismove);
    assert_equal true (Graph.Table.find n3 ismove)

let test2 test_ctxt =
  let instrs = [
    Assem.OPER {Assem.
                 assem = "b `j0";
                 src = [];
                 dst = [];
                 jump = Some [Temp.named_label "l12"]};
    Assem.LABEL {Assem.
                  assem = "l11";
                  lab = Temp.named_label "l11"};  
    Assem.LABEL {Assem.
                  assem = "l12";
                  lab = Temp.named_label "l12"}] in
  let ({Flow.control; def; use; ismove}, [n1; n2; n3]) =
    Make_graph.instrs2graph instrs in
    (* control graph edges *)
    assert_equal ~cmp:cmpnodes [] (Graph.pred n1);
    assert_equal ~cmp:cmpnodes [n3] (Graph.succ n1);
    assert_equal ~cmp:cmpnodes [n3] (Graph.succ n2)

let suite =
  "suite" >:::
  ["test1">:: test1;
   "test2">:: test2]

let _ =
  run_test_tt_main suite
