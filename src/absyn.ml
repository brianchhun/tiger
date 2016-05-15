type pos = int
type symbol = Symbol.t

type field = {
    name: symbol;
    escape: bool ref; 
    ty: symbol;
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
	 | TypeDec of tydec list
			      
 and ty = NameTy of symbol * pos
	| RecordTy of field list
	| ArrayTy of symbol * pos
				
 and oper = PlusOp
	  | MinusOp
	  | TimesOp
	  | DivideOp
	  | EqOp
	  | NeqOp
	  | LtOp
	  | LeOp
	  | GtOp
	  | GeOp
	      
 and fundec = {
     fundec_name: symbol;
     fundec_params: field list;
     fundec_result: (symbol * pos) option;
     fundec_body: exp;
     fundec_pos: pos
   }
		
 and vardec = {
     vardec_name: symbol;
     vardec_escape: bool ref;
     vardec_ty: (symbol * pos) option;
     vardec_init: exp;
     vardec_pos: pos
   }
		
 and tydec = {
     tydec_name: symbol;
     tydec_ty: ty;
     tydec_pos: pos
   }	       
