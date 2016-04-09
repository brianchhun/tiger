type pos = int
type symbol = Symbol.symbol

type field = {
    name: symbol;
    escape: bool ref; 
    typ: symbol;
    pos: pos
  }
		    
type var = SimpleVar of symbol * pos
	 | FieldVar of var * symbol * pos
	 | SubscriptVar of var * exp * pos
					 
 and exp = VarExp of var
	| NilExp
	| IntExp of int
	| StringExp of string * pos
	| CallExp of symbol (* func *) * exp list (* args *) * pos
	| OpExp of exp (* left *) * oper * exp (* right *) * pos
	| RecordExp of (symbol * exp * pos) list (* fields *) * symbol (* typ *) * pos
	| SeqExp of (exp * pos) list
	| AssignExp of var * exp * pos
	| IfExp of exp (* test *) * exp (* then *) * exp option (* else *) * pos
	| WhileExp of exp (* test *) * exp (* body *) * pos
	| ForExp of symbol (* var *) * bool ref (* escape *) *
		      exp (* lo *) * exp (* high *) * exp (* body *) * pos
	| BreakExp of pos
	| LetExp of dec list (* decs *) * exp (* body *) * pos
	| ArrayExp of symbol (* typ *) * exp (* size *) * exp (* init *) * pos
									     
 and dec = FunctionDec of fundec list
	 | VarDec of vardec
	 | TypeDec of typedec list
			      
 and ty = NameTy of symbol * pos
	| RecordTy of field list
	| ArrayTy of symbol * pos
				
 and oper = PlusOp |
	    MinusOp |
	    TimesOp |
	    DivideOp |
	    EqOp |
	    NeqOp |
	    LtOp |
	    LeOp |
	    GtOp |
	    GeOp
						  
 and fundec = {
     fundec__name: symbol;
     params: field list;
     result: (symbol * pos) option;
     body: exp;
     fundec_pos: pos
   }
		
 and vardec = {
     vardec_name: symbol;
     escape: bool ref;
     typ: (symbol * pos) option;
     init: exp;
     vardec_pos: pos
   }
		
 and typedec = {
     typedec_name: symbol;
     ty: ty;
     typedec_pos: pos
   }
