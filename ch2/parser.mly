%token IF
%start prog
%type <string> prog
%%

prog:
	IF { "IF" }
;