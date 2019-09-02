(* OCaml version of Fig 4.8 in Appel.

   A few field names have been changed to avoid warnings about
   duplicate field names and a `typedec` structure is given a 
   name instead of inlining it in the definition of TypeDec.

   The 'functorized' version of this was originally from Gopalan
   Nadathur, but in Standard ML.  Eric Van Wyk has translated it to
   OCaml with some modifications due to the limited nature of
   OCamlYacc and how it handles modules.
 *)

module type ABSYN = sig
  type pos = int
  type symbol

  type var 
    = SimpleVar of symbol * pos
    | FieldVar of var * symbol * pos
    | SubscriptVar of var * exp * pos

   and exp 
     = VarExp of var
     | NilExp
     | IntExp of int
     | StringExp of string * pos
     | CallExp of {func: symbol; args: exp list; pos: pos}
     | OpExp of {left: exp; oper: oper; right: exp; pos: pos}
     | RecordExp of {fields: (symbol * exp * pos) list;
		     typ: symbol; pos: pos}

     | SeqExp of (exp * pos) list

     | AssignExp of {var: var; exp: exp; pos: pos}

     | IfExp of {test: exp; then': exp; else': exp option; pos: pos}

     | WhileExp of {test: exp; body: exp; pos: pos}

     | ForExp of {var: symbol; escape: bool ref;
	          lo: exp; hi: exp; body: exp; pos: pos}

     | BreakExp of pos

     | LetExp of {decs: dec list; body: exp; pos: pos}
               
     | ArrayExp of {typ: symbol; size: exp; init: exp; pos: pos}

   and dec 
     = FunctionDec of fundec list

     | VarDec of {name: symbol;
	          escape: bool ref;
	          typ: (symbol * pos) option;
	          init: exp;
	          pos: pos}

     | TypeDec of typedec list 

   and ty 
     = NameTy of symbol * pos
     | RecordTy of field list
     | ArrayTy of symbol * pos


   and oper = PlusOp | MinusOp | TimesOp | DivideOp
              | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

   and typedec =  {tname: symbol; ty: ty; tpos: pos}

   and field = {fname: symbol; escape: bool ref;
	        typ: symbol; fpos: pos}

   and fundec = {fnname: symbol;
	         params: field list;
	         result: (symbol * pos) option;
	         body: exp;
	         fnpos: pos}
end

module Absyn (S: Symbol.SYMBOL) : (ABSYN with type symbol = S.symbol) 
  = struct

  type pos = int

  type symbol = S.symbol

  type var 
    = SimpleVar of symbol * pos
    | FieldVar of var * symbol * pos
    | SubscriptVar of var * exp * pos

   and exp 
     = VarExp of var
     | NilExp
     | IntExp of int
     | StringExp of string * pos
     | CallExp of {func: symbol; args: exp list; pos: pos}
     | OpExp of {left: exp; oper: oper; right: exp; pos: pos}
     | RecordExp of {fields: (symbol * exp * pos) list;
		     typ: symbol; pos: pos}

     | SeqExp of (exp * pos) list

     | AssignExp of {var: var; exp: exp; pos: pos}

     | IfExp of {test: exp; then': exp; else': exp option; pos: pos}

     | WhileExp of {test: exp; body: exp; pos: pos}

     | ForExp of {var: symbol; escape: bool ref;
	          lo: exp; hi: exp; body: exp; pos: pos}

     | BreakExp of pos

     | LetExp of {decs: dec list; body: exp; pos: pos}

     | ArrayExp of {typ: symbol; size: exp; init: exp; pos: pos}

   and dec 
     = FunctionDec of fundec list

     | VarDec of {name: symbol;
	          escape: bool ref;
	          typ: (symbol * pos) option;
	          init: exp;
	          pos: pos}

     | TypeDec of typedec list 

   and ty 
     = NameTy of symbol * pos
     | RecordTy of field list
     | ArrayTy of symbol * pos


   and oper = PlusOp | MinusOp | TimesOp | DivideOp
              | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

   and typedec =  {tname: symbol; ty: ty; tpos: pos}

   and field = {fname: symbol; escape: bool ref;
	        typ: symbol; fpos: pos}

   and fundec = {fnname: symbol;
	         params: field list;
	         result: (symbol * pos) option;
	         body: exp;
	         fnpos: pos}
end
