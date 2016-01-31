{
open Lexing
exception Unexpected_character of string
exception Eof
}
rule token = parse
       "if"       { Tokens.IF (lexeme_start lexbuf, lexeme_end lexbuf) }
     | ['a'-'z']+ { Tokens.ID (lexeme lexbuf, lexeme_start lexbuf, lexeme_end lexbuf) }
     | "\n"	  { token lexbuf }
     | ","	  { Tokens.COMMA (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "var"	  { Tokens.VAR (lexeme_start lexbuf, lexeme_end lexbuf) }
     | "123"	  { Tokens.INT (123, lexeme_start lexbuf, lexeme_end lexbuf) }
     | eof   	  { raise Eof }
     | _	  { raise (Unexpected_character (lexeme lexbuf)) }