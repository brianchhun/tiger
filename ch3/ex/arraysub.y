%{
#include <stdio.h>
%}
%token ID LBRACKET RBRACKET OF DOT
%%
/*
exp	:	lvalue
	|	arraysub OF exp
	;
lvalue	:	ID
	|	lvalue DOT ID
	|	arraysub
	|	arraysub LBRACKET exp RBRACKET
	;
arraysub	: ID LBRACKET exp RBRACKET
		;
*/
	
exp	:	lvalue
	|	ID arrsub OF exp
	;
lvalue	:	ID
	|	lvalue DOT ID
	|	lvalue arrsub
	;
arrsub	:	LBRACKET exp RBRACKET
	;