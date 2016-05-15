%{
  module A = Absyn
  module S = Symbol
  let pos () = Parsing.symbol_start()
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
    { A.AssignExp ($1, $3, pos()) }
  | NIL
    { A.NilExp }
  | LPAREN expseq RPAREN
    { A.SeqExp $2 }
  | INT
    { A.IntExp $1 }
  | MINUS exp %prec UMINUS
    { A.OpExp (A.IntExp 0, A.MinusOp, $2, pos()) }
  | STRING
    { A.StringExp ($1, pos()) }
  | ID LPAREN funargs RPAREN
    { A.CallExp (S.symbol $1, $3, pos()) }
  | exp PLUS exp
    { A.OpExp ($1, A.PlusOp, $3, pos()) }
  | exp MINUS exp
    { A.OpExp ($1, A.MinusOp, $3, pos()) }
  | exp TIMES exp
    { A.OpExp ($1, A.TimesOp, $3, pos()) }
  | exp DIVIDE exp
    { A.OpExp ($1, A.DivideOp, $3, pos()) }
  | exp EQ exp
    { A.OpExp ($1, A.EqOp, $3, pos()) }
  | exp NEQ exp
    { A.OpExp ($1, A.NeqOp, $3, pos()) }
  | exp LT exp
    { A.OpExp ($1, A.LtOp, $3, pos()) }
  | exp GT exp
    { A.OpExp ($1, A.GtOp, $3, pos()) }
  | exp LE exp
    { A.OpExp ($1, A.LeOp, $3, pos()) }
  | exp GE exp
    { A.OpExp ($1, A.GeOp, $3, pos()) }
  | exp AND exp
    { A.IfExp ($1, $3, Some (A.IntExp 0), pos()) }
  | exp OR exp
    { A.IfExp ($1, A.IntExp 1, Some $3, pos()) }
  | ID LBRACE recfields RBRACE
    { A.RecordExp ($3, S.symbol $1, pos()) }
  | ID LBRACK exp RBRACK OF exp
    { A.ArrayExp (S.symbol $1, $3, $6, pos()) }
  | IF exp THEN exp ELSE exp
    { A.IfExp ($2, $4, Some $6, pos()) }
  | IF exp THEN exp
    { A.IfExp ($2, $4, None, pos()) }
  | WHILE exp DO exp
    { A.WhileExp ($2, $4, pos()) }
  | FOR ID ASSIGN exp TO exp DO exp
    { A.ForExp (S.symbol $2, ref true, $4, $6, $8, pos()) }
  | BREAK
    { A.BreakExp (pos()) }
  | LET decs IN expseq END
    { A.LetExp ($2, A.SeqExp $4, pos()) }
  ;

lval:
    ID lval1
    { $2 (A.SimpleVar (S.symbol $1, pos())) }
  ;
lval1:
    /* empty */
    { fun var -> var }
  | DOT ID lval1
    { fun var -> $3 (A.FieldVar (var, S.symbol $2, pos())) }
  | LBRACK exp RBRACK lval1
    { fun var -> $4 (A.SubscriptVar (var, $2, pos())) }
  ;

expseq:
    /* empty */
    { [] }
  | expseq1
    { $1 }
  ;
expseq1:
    exp
    { [($1, pos())] }
  | exp SEMICOLON expseq1
    { ($1, pos()) :: $3 }
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
    { (S.symbol $1, $3, pos()) }
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
       tydec_pos = pos()} }
  ;
vardec:
    VAR ID ASSIGN exp
    { {A.vardec_name = S.symbol $2;
       vardec_escape = ref true;
       vardec_ty = None;
       vardec_init = $4;
       vardec_pos = pos()} }
  | VAR ID COLON ID ASSIGN exp
    { {A.vardec_name = S.symbol $2;
       vardec_escape = ref true;
       vardec_ty = Some (S.symbol $4, pos());
       vardec_init = $6;
       vardec_pos = pos()} }
  ;
fundec:
    FUNCTION ID LPAREN tyfields RPAREN EQ exp
    { {A.fundec_name = S.symbol $2;
       fundec_params = $4;
       fundec_result = None;
       fundec_body = $7;
       fundec_pos = pos()} }
  | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp
    { {A.fundec_name = S.symbol $2;
       fundec_params = $4;
       fundec_result = Some (S.symbol $7, pos());
       fundec_body = $9;
       fundec_pos = pos()} }
  ;

ty:
    ID
    { A.NameTy (S.symbol $1, pos()) }
  | LBRACE tyfields RBRACE
    { A.RecordTy $2 }
  | ARRAY OF ID
    { A.ArrayTy (S.symbol $3, pos()) }
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
       pos = pos()} }
	;
%%
