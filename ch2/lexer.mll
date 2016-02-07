	{
open Lexing

exception End_of_file
exception Unexpected_character of char
exception Illegal_escape_sequence of string

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
       "while"			{ Tokens.WHILE (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "for"			{ Tokens.FOR (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "to"			{ Tokens.TO (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "break"			{ Tokens.BREAK (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "let"			{ Tokens.LET (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "in"			{ Tokens.IN (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "end"			{ Tokens.END (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "function"		{ Tokens.FUNCTION (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "var"			{ Tokens.VAR (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "type"			{ Tokens.TYPE (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "array"			{ Tokens.ARRAY (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "if"			{ Tokens.IF (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "then"			{ Tokens.THEN (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "else"			{ Tokens.ELSE (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "do"			{ Tokens.DO (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "of"			{ Tokens.OF (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "nil"			{ Tokens.NIL (lexeme_start lexbuf, lexeme_end lexbuf) }
     | ","			{ Tokens.COMMA (lexeme_start lexbuf, lexeme_end lexbuf) }
     | ":"			{ Tokens.COLON (lexeme_start lexbuf, lexeme_end lexbuf) }
     | ";"			{ Tokens.SEMICOLON (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "("			{ Tokens.LPAREN (lexeme_start lexbuf, lexeme_end lexbuf) }
     | ")"			{ Tokens.RPAREN (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "["			{ Tokens.LBRACK (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "]"			{ Tokens.RBRACK (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "{"			{ Tokens.LBRACE (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "}"			{ Tokens.RBRACE (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "."			{ Tokens.DOT (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "+"			{ Tokens.PLUS (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "-"			{ Tokens.MINUS (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "*"			{ Tokens.TIMES (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "/"			{ Tokens.DIVIDE (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "="			{ Tokens.EQ (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "<>"			{ Tokens.NEQ (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "<"			{ Tokens.LT (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "<="			{ Tokens.LE (lexeme_start lexbuf, lexeme_end lexbuf) }
     | ">"			{ Tokens.GT (lexeme_start lexbuf, lexeme_end lexbuf) }
     | ">="			{ Tokens.GE (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "&"			{ Tokens.AND (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "|"			{ Tokens.OR (lexeme_start lexbuf, lexeme_end lexbuf) }
     | ":="			{ Tokens.ASSIGN (lexeme_start lexbuf, lexeme_end lexbuf) }
     | identifier as id		{ Tokens.ID (id, lexeme_start lexbuf, lexeme_end lexbuf) }
     | integer as i		{ Tokens.INT (int_of_string i, lexeme_start lexbuf, lexeme_end lexbuf) }
     | '"'			{ let string_start = lexeme_start lexbuf in
				  let contents = string (Buffer.create 16) lexbuf in
				  Tokens.STRING (contents, string_start, lexeme_end lexbuf) }
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
     | eof			{ raise End_of_file }
     | _ as c			{ Buffer.add_char buf c; string buf lexbuf }
and comment depth = parse
     | "*/"			{ if depth = 0
				  then token lexbuf
				  else comment (depth-1) lexbuf }
     | "/*"			{ comment (depth+1) lexbuf }
     | eof			{ raise End_of_file }
     | _			{ comment depth lexbuf }