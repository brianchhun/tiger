/* Palindromes over {a,b} */
%{
#include <stdio.h>
%}
%token A B
%%
p	:	
	|	A
	|	B
	|	A p A
	|	B p B
	;
%%
int main() {
    return yyparse();
}
int yyerror(char *s) { return fprintf(stderr, "%s\n", s); }
