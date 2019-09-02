(* The orinal Standard ML version of this file was written by
   Gopalan Nadathur.

   This translation to OCaml is by Eric Van Wyk.
 *)

module type ENV = sig
  type unique = unit ref

  type symbol

  type 'a table

  type pos = int

  type ty = RECORD of (symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
          | NAME of symbol * ty option ref
          | UNIT
          | ERROR

  type errType =  UndeclaredVar of string
                | UndeclaredType of string
                | UnexpectedType of ty * ty
                | FuncNameAsVarName of string
                | VarNameAsFuncName of string
                | TooFewArgs of string
                | TooManyArgs of string
                | IncorrectType of ty
                | BreakNotAllowed
                | Duplicates of string
                | Circularity 
                | InvalidAssign of string
                | Unclassified of string

  type err = pos * errType * string

  type fnvar = VarEntry of {ty: ty; loopvar: bool}
             | FunEntry of {formals: ty list; result: ty}

  val base_tenv : ty table       (* predefined types *)

  val base_venv : fnvar table    (* predefined functions *)
end


module Env (S: Symbol.SYMBOL) 
         : (ENV with type symbol = S.symbol and type 'a table = 'a S.table)
  = struct
  type unique = unit ref

  type symbol = S.symbol

  type 'a table = 'a S.table

  type pos = int

  type ty = RECORD of (symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
          | NAME of symbol * ty option ref
          | UNIT
          | ERROR

  type errType =  UndeclaredVar of string
                | UndeclaredType of string
                | UnexpectedType of ty * ty
                | FuncNameAsVarName of string
                | VarNameAsFuncName of string
                | TooFewArgs of string
                | TooManyArgs of string
                | IncorrectType of ty
                | BreakNotAllowed
                | Duplicates of string
                | Circularity 
                | InvalidAssign of string
                | Unclassified of string

  type err = pos * errType * string

  type fnvar = VarEntry of {ty: ty; loopvar: bool}
             | FunEntry of {formals: ty list; result: ty}

 
  (* predefined types *)
  let base_tenv : ty table = 
    S.enter 
      (S.enter S.empty (S.symbol "string") STRING)
      (S.symbol "int") 
      INT


  (* predefined functions; incomplete and should be completed using the
     list on page 519 *)
  let base_venv : fnvar table =
    let f t (s, fnv) = S.enter t s fnv in
    let entries = 
      [  (S.symbol "print",
          FunEntry {formals=[STRING]; result=UNIT});

         (S.symbol "flush"),
         (FunEntry {formals=[]; result=UNIT});

         (S.symbol "getchar"),
         (FunEntry {formals=[]; result=STRING});

         (S.symbol "ord"),
         (FunEntry {formals=[STRING]; result=INT});

         (S.symbol "chr"),
         (FunEntry {formals=[INT]; result=STRING});

         (S.symbol "size"),
         (FunEntry {formals=[STRING]; result=INT});

         (S.symbol "substring"),
         (FunEntry {formals=[STRING; INT; INT]; result=STRING})
      ]
    in
    List.fold_left f S.empty entries

end




