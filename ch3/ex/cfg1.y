%{
#include <stdio.h>
%}

%token X Y
%%
/* ((xy*x)|(yx*y))? */
re:
	| xyx
	| yxy
	;
	
xyx:	X ys X

yxy:	Y xs Y

ys:
	| ys Y
	;

xs:
	| xs X
	;
%%
int main() {
    return yyparse();
}
int yyerror(char *s) { return fprintf(stderr, "%s\n", s); }
