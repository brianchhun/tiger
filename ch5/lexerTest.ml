open OUnit
module P = Parser

let tokenize str =
  let lexbuf = Lexing.from_string str in
  let rec loop () =
    match Lexer.token lexbuf with
    | P.EOF -> []
    | token -> token :: loop () in
  loop ()

let ids = "ids">:::
	    List.map (fun (str, expected) ->
		str>:: (fun () -> assert_equal (tokenize str) expected))
		     [("a", [P.ID "a"]);
		      ("a1", [P.ID "a1"]);
		      ("a_b", [P.ID "a_b"])
		     ]

let ints = "ints">:::
	     List.map (fun (str, expected) ->
		 str>:: (fun () -> assert_equal (tokenize str) expected))
		      [("1", [P.INT 1]);
		       ("12", [P.INT 12])
		      ]

let strings = "strings">:::[
      "strings">::
	(fun () ->
	  assert_equal
	    [P.STRING "Hello World"]
	    (tokenize "\"Hello World\""));
      "escape char">::
	(fun () ->
	  assert_equal
	    [P.STRING "\\\n\t\""]
	    (tokenize "\"\\\\\\n\\t\\\"\""));
      "bad escape char">::
	(fun () ->
	  assert_raises
	    (Lexer.Illegal_escape_sequence "\\x")
	    (fun () -> tokenize "\"\\x\""));
      "escape control">::
	(fun () ->
	  assert_equal
	    [P.STRING "\x00\x03\x01\x1d\x1f"]
	    (tokenize "\"\\^@\\^C\\^A\\^]\\^_\""));
      "bad escape control">::
	(fun () ->
	  assert_raises
	    (Lexer.Illegal_escape_sequence "\\^+")
	    (fun () -> tokenize "\"\\^+\""));
      "escape code">::
	(fun () ->
	  assert_equal
	    [P.STRING "\000\001\255"]
	    (tokenize "\"\\000\\001\\255\""));
      "bad escape code">::
	(fun () ->
	  assert_raises
	    (Lexer.Illegal_escape_sequence "\\256")
	    (fun () -> tokenize "\"\\256\""));
      "ignored escape sequence">::
	(fun () ->
	  assert_equal
	    [P.STRING "Hello World"]
	    (tokenize "\"Hello\\\\n   \\t\\r\\ World\""));
      "unterminated escape sequence">::
	(fun () ->
	  assert_raises
	    Lexer.Unterminated_string
	    (fun () -> tokenize "\"abc\\\\n\\n"));
    ]

let comments = "comments">:::[
      "comments">::
	(fun () ->
	  assert_equal
	    [P.ID "x"]
	    (tokenize "/* xy\\nzy */ x"));
      "nested">::
	(fun () ->
	  assert_equal
	    [P.ID "x"]
	    (tokenize "/* x /* zy /**/ */ x */ x"));
      "unterminated">::
	(fun () ->
	  assert_raises
	    Lexer.Unterminated_comment
	    (fun () -> tokenize "/* /* hello xx */"));
    ]

let progs = "progs">:::[
      "test1">::
	(fun () ->
	  assert_equal
	    [
	      P.LET;
	      P.TYPE;
	      P.ID "arrtype";
	      P.EQ;
	      P.ARRAY;
	      P.OF;
	      P.ID "int";
	      P.VAR;
	      P.ID "arr1";
	      P.COLON;
	      P.ID "arrtype";
	      P.ASSIGN;
	      P.ID "arrtype";
	      P.LBRACK;
	      P.INT 10;
	      P.RBRACK;
	      P.OF;
	      P.INT 0;
	      P.IN;
	      P.ID "arr1";
	      P.END
	    ]
	    (tokenize "
let
	type  arrtype = array of int
	var arr1:arrtype := arrtype [10] of 0
in
	arr1
end"));
      "test4">::
	(fun () ->
	  assert_equal
	    [
	      P.LET;
	      P.FUNCTION;
	      P.ID "nfactor";
	      P.LPAREN;
	      P.ID "n";
	      P.COLON;
	      P.ID "int";
	      P.RPAREN;
	      P.COLON;
	      P.ID "int";
	      P.EQ;
	      P.IF;
	      P.ID "n";
	      P.EQ;
	      P.INT 0;
	      P.THEN;
	      P.INT 1;
	      P.ELSE;
	      P.ID "n";
	      P.TIMES;
	      P.ID "nfactor";
	      P.LPAREN;
	      P.ID "n";
	      P.MINUS;
	      P.INT 1;
	      P.RPAREN;
	      P.IN;
	      P.ID "nfactor";
	      P.LPAREN;
	      P.INT 10;
	      P.RPAREN;
	      P.END
	    ]
	    (tokenize "
/* define a recursive function */
let

/* calculate n! */
function nfactor(n: int): int =
		if  n = 0 
			then 1
			else n * nfactor(n-1)

in
	nfactor(10)
end"))
    ]
			     
let suite = "suite">:::
	      [ids;
	       ints;
	       strings;
	       comments;
	       progs]

let _ =
  run_test_tt_main suite
