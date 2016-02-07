type token =
  | TYPE of int * int
  | VAR of int * int
  | FUNCTION of int * int 
  | BREAK of int * int 
  | OF of int * int 
  | END of int * int 
  | IN of int * int 
  | NIL of int * int 
  | LET of int * int 
  | DO of int * int 
  | TO of int * int 
  | FOR of int * int 
  | WHILE of int * int 
  | ELSE of int * int 
  | THEN of int * int 
  | IF of int * int 
  | ARRAY of int * int 
  | ASSIGN of int * int 
  | OR of int * int 
  | AND of int * int 
  | GE of int * int 
  | GT of int * int 
  | LE of int * int 
  | LT of int * int 
  | NEQ of int * int 
  | EQ of int * int 
  | DIVIDE of int * int 
  | TIMES of int * int 
  | MINUS of int * int 
  | PLUS of int * int 
  | DOT of int * int 
  | RBRACE of int * int 
  | LBRACE of int * int 
  | RBRACK of int * int 
  | LBRACK of int * int 
  | RPAREN of int * int 
  | LPAREN of int * int 
  | SEMICOLON of int * int 
  | COLON of int * int 
  | COMMA of int * int 
  | STRING of string * int * int
  | INT of int * int * int
  | ID of string * int * int
  | EOF of int * int 

let string_of_token = function
  | TYPE(i,j)		-> "TYPE   " ^ (string_of_int i)
  | VAR(i,j)		-> "VAR   " ^ (string_of_int i)
  | FUNCTION(i,j)	-> "FUNCCTION   " ^ (string_of_int i)
  | BREAK(i,j)		-> "BREAK   " ^ (string_of_int i)
  | OF(i,j)		-> "OF   " ^ (string_of_int i)
  | END(i,j)		-> "END   " ^ (string_of_int i)
  | IN(i,j)		-> "IN   " ^ (string_of_int i)
  | NIL(i,j)		-> "NIL   " ^ (string_of_int i)
  | LET(i,j)		-> "LET   " ^ (string_of_int i)
  | DO(i,j)		-> "DO   " ^ (string_of_int i)
  | TO(i,j)		-> "TO   " ^ (string_of_int i)
  | FOR(i,j)		-> "FOR   " ^ (string_of_int i)
  | WHILE(i,j)		-> "WHILE   " ^ (string_of_int i)
  | ELSE(i,j)		-> "ELSE   " ^ (string_of_int i)
  | THEN(i,j)		-> "THEN   " ^ (string_of_int i)
  | IF(i,j)		-> "IF   " ^ (string_of_int i)
  | ARRAY(i,j)		-> "ARRAY   " ^ (string_of_int i)
  | ASSIGN(i,j)		-> "ASSIGN   " ^ (string_of_int i)
  | OR(i,j)		-> "OR   " ^ (string_of_int i)
  | AND(i,j)		-> "AND   " ^ (string_of_int i)
  | GE(i,j)		-> "GE   " ^ (string_of_int i)
  | GT(i,j)		-> "GT   " ^ (string_of_int i)
  | LE(i,j)		-> "LE   " ^ (string_of_int i)
  | LT(i,j)		-> "LT   " ^ (string_of_int i)
  | NEQ(i,j)		-> "NEQ   " ^ (string_of_int i)
  | EQ(i,j)		-> "EQ   " ^ (string_of_int i)
  | DIVIDE(i,j)		-> "DIVIDE   " ^ (string_of_int i)
  | TIMES(i,j)		-> "TIMES   " ^ (string_of_int i)
  | MINUS(i,j)		-> "MINUS   " ^ (string_of_int i)
  | PLUS(i,j)		-> "PLUS   " ^ (string_of_int i)
  | DOT(i,j)		-> "DOT   " ^ (string_of_int i)
  | RBRACE(i,j)		-> "RBRACE   " ^ (string_of_int i)
  | LBRACE(i,j)		-> "LBRACE   " ^ (string_of_int i)
  | RBRACK(i,j)		-> "RBRACK   " ^ (string_of_int i)
  | LBRACK(i,j)		-> "LBRACK   " ^ (string_of_int i)
  | RPAREN(i,j)		-> "RPAREN   " ^ (string_of_int i)
  | LPAREN(i,j)		-> "LPAREN   " ^ (string_of_int i)
  | SEMICOLON(i,j)	-> "SEMICOLON   " ^ (string_of_int i)
  | COLON(i,j)		-> "COLON   " ^ (string_of_int i)
  | COMMA(i,j)		-> "COMMA   " ^ (string_of_int i)
  | STRING(s,i,j)	-> "STRING("^s^")     " ^ (string_of_int i)
  | INT(c,i,j)		-> "INT("^(string_of_int c)^")   " ^ (string_of_int i)
  | ID(s,i,j)		-> "ID("^s^")     " ^ (string_of_int i)
  | EOF(i,j)		-> "EOF   " ^ (string_of_int i)

