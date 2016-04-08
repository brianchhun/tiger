%{
int yydebug = 1;
%}
%token ID PLUS MINUS TIMES DIV
%left TIMES DIV
%left PLUS MINUS
%%
E	:	ID
	|	E PLUS E
	|	E TIMES E
	|	E MINUS E
	|	E DIV E
	;
%%