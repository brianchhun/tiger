%{
#include "y.tab.h"
%}

%%
\(	return LPAREN;
\)	return RPAREN;
\[	return LBRACK;
\]	return RBRACK;
.	{ printf("Unrecogzined character: %s\n", yytext); }
%%
