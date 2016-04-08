/* Balanced parentheses and square brackets */
%{
#include <stdio.h>
%}
%token LPAREN RPAREN LBRACK RBRACK
%%
b		:	LPAREN list RPAREN
		|	LBRACK list RBRACK
list	:
		|	b list
		;
%%
int main() {
    return yyparse();
}
int yyerror(char *s) { return fprintf(stderr, "%s\n", s); }
