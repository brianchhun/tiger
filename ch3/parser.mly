%{
%}
%token <int> INT
%token <string> ID
%token <string> STRING
%token AND
%token ARRAY
%token ASSIGN
%token BREAK
%token COLON
%token COMMA
%token DIVIDE
%token DO
%token DOT
%token ELSE
%token END
%token EQ
%token FOR
%token FUNCTION
%token GE
%token GT
%token IF
%token IN
%token LBRACE
%token LBRACK
%token LE
%token LET
%token LPAREN
%token LT
%token MINUS
%token NEQ
%token NIL
%token OF
%token OR
%token PLUS
%token RBRACE
%token RBRACK
%token RPAREN
%token SEMICOLON
%token THEN
%token TIMES
%token TO
%token TYPE
%token VAR
%token WHILE

%start program
%type <unit> program

%nonassoc ASSIGN
%right OR
%right AND
%nonassoc EQ NEQ GT LT GE LE
%left PLUS MINUS
%left TIMES DIVIDE
%left UMINUS

%%
program		: exp					{}
		;
		
exp		: lval					{}
		| lval ASSIGN exp			{}
		| NIL					{}
		| LPAREN expseq RPAREN			{}
		| INT					{}
		| STRING				{}
		| MINUS exp	%prec UMINUS		{}
		| app					{}
		| exp PLUS exp				{}
		| exp MINUS exp				{}
		| exp TIMES exp				{}
		| exp DIVIDE exp			{}
		| exp EQ exp				{}
		| exp NEQ exp				{}
		| exp LT exp				{}
		| exp GT exp				{}
		| exp LE exp				{}
		| exp GE exp				{}
		| exp AND exp				{}
		| exp OR exp				{}
		| ID LBRACE recfields RBRACE		{}
		| ID LBRACK exp RBRACK OF exp    	{}
		| IF exp THEN exp ELSE exp		{}
		| IF exp THEN exp			{}
		| WHILE exp DO exp			{}
		| FOR ID ASSIGN exp TO exp DO exp	{}
		| BREAK					{}
		| LET decs IN expseq END		{}
		;

lval		: ID lval1				{}
		;
lval1		: /* empty */				{}
		| DOT ID lval1				{}
		| LBRACK exp RBRACK lval1		{}
		; 

expseq		: /* empty */				{}
		| expseq1				{}
		;
expseq1		: exp					{}
		| expseq1 SEMICOLON exp			{}
	  	;
	
app		: ID LPAREN args RPAREN			{}
		;
args		: /* empty */				{}
		| args1	   				{}
		;
args1		: exp					{}
		| args1 COMMA exp			{}
		;
		
recfields	: /* empty */				{}
		| recfields1				{}
		;
recfields1	: recfield				{}
		| recfields1 COMMA recfield		{}
		;
recfield	: ID EQ exp				{}
		;
		
decs		: /* empty */				{}
		| dec decs				{}
		;
dec		: tydec					{}
		| vardec				{}
		| fundec				{}
		;
	
tydec		: TYPE ID EQ ty				{}
		;
vardec		: VAR ID ASSIGN exp			{}
		| VAR ID COLON ID ASSIGN exp		{}
		;
	
fundec		: FUNCTION ID LPAREN tyfields RPAREN EQ exp		{}
		| FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp	{}
		;
ty		: ID					{}
		| LBRACE tyfields RBRACE		{}
		| ARRAY OF ID				{}
		;
tyfields	: /* empty */				{}
		| tyfields1				{}
		;
tyfields1	: ID COLON ID				{}
		| tyfields1 COMMA ID COLON ID	{}
		;
%%