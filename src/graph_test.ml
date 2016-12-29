open OUnit2

let cmpnodes = List.for_all2 Graph.eq
    
let test1 test_ctxt =
  let g = Graph.new_graph () in
    Graph.new_node g;
    Graph.new_node g;
    assert_equal (List.length (Graph.nodes g)) 2

let test2 test_ctxt =
  let g = Graph.new_graph () in
  let n1 = Graph.new_node g in
  let n2 = Graph.new_node g in
  let n3 = Graph.new_node g in
    Graph.mk_edge n1 n3;
    Graph.mk_edge n2 n3;
    assert_equal ~cmp:cmpnodes (Graph.succ n1) [n3];
    assert_equal ~cmp:cmpnodes (Graph.succ n2) [n3];
    assert_equal ~cmp:cmpnodes (Graph.pred n3) [n2; n1];
    Graph.rm_edge n2 n3;
    assert_equal ~cmp:cmpnodes (Graph.succ n2) [];
    assert_equal ~cmp:cmpnodes (Graph.pred n3) [n1]

let test3 test_ctxt =
    let g = Graph.new_graph () in
    let n1 = Graph.new_node g in
    let n2 = Graph.new_node g in
    let n3 = Graph.new_node g in
    let n4 = Graph.new_node g in
      Graph.mk_edge n1 n2;
      Graph.mk_edge n2 n3;
      Graph.mk_edge n3 n4;
      Graph.mk_edge n4 n1;
      assert_equal ~cmp:cmpnodes (Graph.adj n1) [n4; n2];
      assert_equal ~cmp:cmpnodes (Graph.adj n2) [n1; n3];
      assert_equal ~cmp:cmpnodes (Graph.adj n3) [n2; n4];
      assert_equal ~cmp:cmpnodes (Graph.adj n4) [n3; n1];
      Graph.rm_edge n2 n3;
      assert_equal ~cmp:cmpnodes (Graph.adj n2) [n1];
      assert_equal ~cmp:cmpnodes (Graph.adj n3) [n4]

let suite =
  "suite" >:::
  ["test1">:: test1;
   "test2">:: test2;
   "test3">:: test3]

let _ =
  run_test_tt_main suite
