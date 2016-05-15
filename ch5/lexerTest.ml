open OUnit

let tokenize str = Lexer.token (Lexing.from_string str)

let string_of_token = function
  | Parser.STRING str -> str
  | _ -> ""

let ids = "ids">:::
	    List.map (fun (str, expected) ->
		str>:: (fun () -> assert_equal (tokenize str) expected))
		     [("a", Parser.ID "a");
		      ("a1", Parser.ID "a1");
		      ("a_b", Parser.ID "a_b")]

let ints = "ints">:::
	     List.map (fun (str, expected) ->
		 str>:: (fun () -> assert_equal (tokenize str) expected))
		      [("1", Parser.INT 1);
		       ("12", Parser.INT 12)]

let strings = "strings">:::[
      "strings">::
	(fun () ->
	  assert_equal
	    (Parser.STRING "Hello World")
	    (tokenize "\"Hello World\""));
      "ignored sequence">::
	(fun () ->
	  assert_equal
	    (Parser.STRING "Hello World")
	    (tokenize "\"Hello\\\\n   \\t\\r\\ World\""));
      "escaped sequence">::
	(fun () ->
	  assert_equal
	    (Parser.STRING "\\\n\t\"")
	    (tokenize "\"\\\\\\n\\t\\\"\""));
      "control char">::
	(fun () ->
	  assert_equal
	    (Parser.STRING "\x00\x03\x01\x1d\x1f")
	    (tokenize "\"\\^@\\^C\\^A\\^]\\^_\""));
      "code char">::
	(fun () ->
	  assert_equal
	    (Parser.STRING "\000\001\255")
	    (tokenize "\"\\000\\001\\255\""))
    ]
			     
let suite = "suite">:::
	      [ids;
	       ints;
	       strings]

let _ =
  run_test_tt_main suite
