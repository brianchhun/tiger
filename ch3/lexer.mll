{
open Parser
open Lexing

exception End_of_file
exception Unexpected_character of char
exception Illegal_escape_sequence of string
exception Unterminated_string
exception Unterminated_comment

let unescape_char = function
  | '\\' -> '\\'
  | 'n'  -> '\n'
  | 't'  -> '\t'
  | '"'  -> '"'
  | c    -> raise (Illegal_escape_sequence ("\\" ^ String.make 1 c))

let unescape_control c =
  let code = Char.code c in
  if code >= 64 && code <= 95
  then Char.chr (code-64)
  else raise (Illegal_escape_sequence ("\\^" ^ String.make 1 c))

let unescape_code s =
  let code = int_of_string s in
  if code >= 0 && code <= 255
  then Char.chr code
  else raise (Illegal_escape_sequence ("\\" ^ s))
}

let identifier = ['a'-'z' 'A'-'Z'] (['a'-'z' 'A'-'Z'] | ['0'-'9'] | '_') *
let integer = ['0'-'9'] +

rule token = parse
       "while"			{ WHILE }
     | "for"			{ FOR }
     | "to"			{ TO }
     | "break"			{ BREAK }
     | "let"			{ LET }
     | "in"			{ IN }
     | "end"			{ END }
     | "function"		{ FUNCTION }
     | "var"			{ VAR }
     | "type"			{ TYPE }
     | "array"			{ ARRAY }
     | "if"			{ IF }
     | "then"			{ THEN }
     | "else"			{ ELSE }
     | "do"			{ DO }
     | "of"			{ OF }
     | "nil"			{ NIL }
     | ","			{ COMMA }
     | ":"			{ COLON }
     | ";"			{ SEMICOLON }
     | "("			{ LPAREN }
     | ")"			{ RPAREN }
     | "["			{ LBRACK }
     | "]"			{ RBRACK }
     | "{"			{ LBRACE }
     | "}"			{ RBRACE }
     | "."			{ DOT }
     | "+"			{ PLUS }
     | "-"			{ MINUS }
     | "*"			{ TIMES }
     | "/"			{ DIVIDE }
     | "="			{ EQ }
     | "<>"			{ NEQ }
     | "<"			{ LT }
     | "<="			{ LE }
     | ">"			{ GT }
     | ">="			{ GE }
     | "&"			{ AND }
     | "|"			{ OR }
     | ":="			{ ASSIGN }
     | identifier as id		{ ID id }
     | integer as i		{ INT (int_of_string i) }
     | '"'			{ let contents = string (Buffer.create 16) lexbuf in
				  STRING contents }
     | "/*"			{ comment 0 lexbuf }
     | [' ' '\n' '\t' '\r']	{ token lexbuf }
     | eof			{ raise End_of_file }
     | _ as c			{ raise (Unexpected_character c) }
and string buf = parse
     | '\\' (['\\' 'n' 't' '"'] as c)
       { Buffer.add_char buf (unescape_char c); string buf lexbuf }
     | '\\' '^' (_ as c)
       { Buffer.add_char buf (unescape_control c); string buf lexbuf }
     | '\\' (['0'-'9'] ['0'-'9'] ['0'-'9'] as s)
       { Buffer.add_char buf (unescape_code s); string buf lexbuf }
     | '\\' [' ' '\n' '\t' '\r'] + '\\'
       { string buf lexbuf }
     | '"'			{ Buffer.contents buf }
     | eof			{ raise Unterminated_string }
     | _ as c			{ Buffer.add_char buf c; string buf lexbuf }
and comment depth = parse
     | "*/"			{ if depth = 0
				  then token lexbuf
				  else comment (depth-1) lexbuf }
     | "/*"			{ comment (depth+1) lexbuf }
     | eof			{ raise Unterminated_comment }
     | _			{ comment depth lexbuf }