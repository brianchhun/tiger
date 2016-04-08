%{
#include "y.tab.h"
%}

%%
a	return A;
b	return B;
.	{ printf("Unrecogzined character: %s\n", yytext); }
%%
