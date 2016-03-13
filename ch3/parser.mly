%{
%}
%token <string> ID
%token <int> INT
%token <string> STRING
%token COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK
%token LBRACE RBRACE DOT
%token PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE
%token AND OR ASSIGN
%token ARRAY IF THEN ELSE WHILE FOR TO DO LET IN END OF
%token BREAK NIL
%token FUNCTION VAR TYPE
%start program
%type <unit> program
%%

program	: exp	{}
exp	:
	  INT	{}
	| INT PLUS INT {}
	| INT MINUS INT {}
	| INT TIMES INT {}
	| INT DIVIDE INT {}
%%