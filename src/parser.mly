%{
  module A = Absyn
  module S = Symbol
  module P = Parsing
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
%token EOF
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
%type <Absyn.exp> program

%nonassoc ASSIGN
%right OR
%right AND
%nonassoc EQ NEQ GT LT GE LE
%left PLUS MINUS
%left TIMES DIVIDE
%left UMINUS

%%
program:
    exp EOF
    { $1 }
  ;

exp:
    lval
    { A.VarExp $1 }
  | lval ASSIGN exp
    { A.AssignExp ($1, $3, ref true, P.rhs_start 2) }
  | NIL
    { A.NilExp }
  | LPAREN expseq RPAREN
    { A.SeqExp $2 }
  | INT
    { A.IntExp $1 }
  | MINUS exp %prec UMINUS
    { A.OpExp (A.IntExp 0, A.MinusOp, $2, P.rhs_start 1) }
  | STRING
    { A.StringExp ($1, P.rhs_start 1) }
  | ID LPAREN funargs RPAREN
    { A.CallExp (S.symbol $1, $3, P.rhs_start 1) }
  | exp PLUS exp
    { A.OpExp ($1, A.PlusOp, $3, P.rhs_start 2) }
  | exp MINUS exp
    { A.OpExp ($1, A.MinusOp, $3, P.rhs_start 2) }
  | exp TIMES exp
    { A.OpExp ($1, A.TimesOp, $3, P.rhs_start 2) }
  | exp DIVIDE exp
    { A.OpExp ($1, A.DivideOp, $3, P.rhs_start 2) }
  | exp EQ exp
    { A.OpExp ($1, A.EqOp, $3, P.rhs_start 2) }
  | exp NEQ exp
    { A.OpExp ($1, A.NeqOp, $3, P.rhs_start 2) }
  | exp LT exp
    { A.OpExp ($1, A.LtOp, $3, P.rhs_start 2) }
  | exp GT exp
    { A.OpExp ($1, A.GtOp, $3, P.rhs_start 2) }
  | exp LE exp
    { A.OpExp ($1, A.LeOp, $3, P.rhs_start 2) }
  | exp GE exp
    { A.OpExp ($1, A.GeOp, $3, P.rhs_start 2) }
  | exp AND exp
    { A.IfExp ($1, $3, Some (A.IntExp 0), P.rhs_start 2) }
  | exp OR exp
    { A.IfExp ($1, A.IntExp 1, Some $3, P.rhs_start 2) }
  | ID LBRACE recfields RBRACE
    { A.RecordExp ($3, S.symbol $1, P.rhs_start 1) }
  | ID LBRACK exp RBRACK OF exp
    { A.ArrayExp (S.symbol $1, $3, $6, P.rhs_start 1) }
  | IF exp THEN exp ELSE exp
    { A.IfExp ($2, $4, Some $6, P.rhs_start 1) }
  | IF exp THEN exp
    { A.IfExp ($2, $4, None, P.rhs_start 1) }
  | WHILE exp DO exp
    { A.WhileExp ($2, $4, P.rhs_start 1) }
  | FOR ID ASSIGN exp TO exp DO exp
    { A.ForExp (S.symbol $2, ref true, $4, $6, $8, P.rhs_start 1) }
  | BREAK
    { A.BreakExp (P.rhs_start 1) }
  | LET decs IN expseq END
    { A.LetExp ($2, A.SeqExp $4, P.rhs_start 1) }
  ;

lval:
    ID lval1
    { $2 (A.SimpleVar (S.symbol $1, P.rhs_start 1)) }
  ;
lval1:
    /* empty */
    { fun var -> var }
  | DOT ID lval1
    { fun var -> $3 (A.FieldVar (var, S.symbol $2, P.rhs_start 1)) }
  | LBRACK exp RBRACK lval1
    { fun var -> $4 (A.SubscriptVar (var, $2, P.rhs_start 1)) }
  ;

expseq:
    /* empty */
    { [] }
  | expseq1
    { $1 }
  ;
expseq1:
    exp
    { [($1, P.rhs_start 1)] }
  | exp SEMICOLON expseq1
    { ($1, P.rhs_start 1) :: $3 }
  ;

funargs:
    /* empty */
    { [] }
  | funargs1
    { $1 }
  ;
funargs1:
    exp
    { [$1] }
  | exp COMMA funargs
    { $1 :: $3 }
  ;

recfields:
    /* empty */
    { [] }
  | recfields1
    { $1 }
  ;
recfields1:
    recfield
    { [$1] }
  | recfield COMMA recfields1
    { $1  :: $3 }
  ;
recfield:
    ID EQ exp
    { (S.symbol $1, $3, P.rhs_start 1) }
  ;

decs:
    /* empty */
    { [] }
  | dec decs
    { match $1 , $2 with
      | A.TypeDec [tydec] , A.TypeDec tydecs :: decs ->
        A.TypeDec (tydec :: tydecs) :: decs
      | A.FunctionDec [fundec] , A.FunctionDec fundecs :: decs ->
        A.FunctionDec (fundec :: fundecs) :: decs
      | dec , decs -> dec :: decs }
  ;
dec:
    tydec
    { A.TypeDec [$1] }
  | vardec
    { A.VarDec $1 }
  | fundec
    { A.FunctionDec [$1] }
  ;

tydec:
    TYPE ID EQ ty
    { {A.tydec_name = S.symbol $2;
       tydec_ty = $4;
       tydec_pos = P.rhs_start 1} }
  ;
vardec:
    VAR ID ASSIGN exp
    { {A.vardec_name = S.symbol $2;
       vardec_escape = ref true;
       vardec_ty = None;
       vardec_init = $4;
       vardec_pos = P.rhs_start 1} }
  | VAR ID COLON ID ASSIGN exp
    { {A.vardec_name = S.symbol $2;
       vardec_escape = ref true;
       vardec_ty = Some (S.symbol $4, P.rhs_start 4);
       vardec_init = $6;
       vardec_pos = P.rhs_start 1} }
  ;
fundec:
    FUNCTION ID LPAREN tyfields RPAREN EQ exp
    { {A.fundec_name = S.symbol $2;
       fundec_params = $4;
       fundec_result = None;
       fundec_body = $7;
       fundec_pos = P.rhs_start 1} }
  | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp
    { {A.fundec_name = S.symbol $2;
       fundec_params = $4;
       fundec_result = Some (S.symbol $7, P.rhs_start 7);
       fundec_body = $9;
       fundec_pos = P.rhs_start 1} }
  ;

ty:
    ID
    { A.NameTy (S.symbol $1, P.rhs_start 1) }
  | LBRACE tyfields RBRACE
    { A.RecordTy $2 }
  | ARRAY OF ID
    { A.ArrayTy (S.symbol $3, P.rhs_start 1) }
  ;
tyfields:
    /* empty */
    { [] }
  | tyfields1
    { $1 }
  ;
tyfields1:
    tyfield
    { [$1] }
  | tyfield COMMA tyfields1
    { $1 :: $3 }
  ;
tyfield:
    ID COLON ID
    { {A.name = S.symbol $1;
       escape = ref true;
       ty = S.symbol $3;
       pos = P.rhs_start 1} }
	;
%%
