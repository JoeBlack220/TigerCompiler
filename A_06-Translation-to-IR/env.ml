(* The orinal Standard ML version of this file was written by
   Gopalan Nadathur.

   This translation to OCaml is by Eric Van Wyk.
 *)

module type ENV = sig
  type unique = unit ref
  type symbol
  type 'a table
  type pos = int

  (* A_05: The following 3 types are new. *)
  type access
  type level
  type label

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

  (* A_05: Note new fields in records below. *)
  type fnvar = VarEntry of {access: access; ty: ty; loopvar: bool}
             | FunEntry of {level: level; label: label; 
                            formals: ty list; result: ty}

  val base_tenv : ty table       (* predefined types *)

  val base_venv : fnvar table    (* predefined functions *)
end

(* A_05: Renamed symbol module from `S` to `Sy` to match other modules.
   Also adding `Tm: TEMP` and `Tr: TRANSLATE` to the `Env` functor. *)
module Env (Sy: Symbol.SYMBOL)
           (Tm: Temp.TEMP)
           (Tr: Translate.TRANSLATE with type label = Tm.label)
         : (ENV with type symbol = Sy.symbol 
                 and type 'a table = 'a Sy.table
                 and type label = Tm.label 
                 and type label = Tr.label
                 and type level = Tr.level
                 and type access = Tr.access )
  = struct
  type unique = unit ref
  type symbol = Sy.symbol
  type 'a table = 'a Sy.table
  type pos = int

  type access = Tr.access
  type label = Tm.label
  type level = Tr.level

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

  (* A_05: Note new fields in records below. *)
  type fnvar = VarEntry of {access: access; ty: ty; loopvar: bool}
             | FunEntry of {level: level; label: label; 
                            formals: ty list; result: ty}

 
  (* predefined types *)
  let base_tenv : ty table = 
    Sy.enter 
      (Sy.enter Sy.empty (Sy.symbol "string") STRING)
      (Sy.symbol "int") 
      INT


  (* predefined functions; incomplete and should be completed using the
     list on page 519 *)
  (* A_05: fill in the new fields to FunEntry seen in declarations above. *)
  let base_venv : fnvar table =
    let f t (s, fnv) = Sy.enter t s fnv in
    let entries = 
      [  (Sy.symbol "print",
          let name = Tm.named_label "$Print$$" in
          FunEntry {level=Tr.new_level { Tr.parent=Tr.outermost; 
                                         name=name;
                                         formals=[false] } ;
                    label=name;
                    formals=[STRING]; result=UNIT});

         (Sy.symbol "flush",
          let name = Tm.named_label "$Flush$$" in
          FunEntry {level=Tr.new_level { Tr.parent=Tr.outermost; 
                                         name=name;
                                         formals=[] } ;
                    label=name;
                    formals=[]; result=UNIT});

         (Sy.symbol "getchar",
          let name = Tm.named_label "$GetChar$$" in
          FunEntry {level=Tr.new_level { Tr.parent=Tr.outermost; 
                                         name=name;
                                         formals=[] } ;
                    label=name;
                    formals=[]; result=STRING});

         (Sy.symbol "ord",
          let name = Tm.named_label "$Ord$$" in
          FunEntry {level=Tr.new_level { Tr.parent=Tr.outermost; 
                                         name=name;
                                         formals=[false] } ;
                    label=name;
                    formals=[STRING]; result=INT});

         (Sy.symbol "chr",
          let name = Tm.named_label "$Chr$$" in
          FunEntry {level=Tr.new_level { Tr.parent=Tr.outermost; 
                                         name=name;
                                         formals=[false] } ;
                    label=name;
                    formals=[INT]; result=STRING});

         (Sy.symbol "size",
          let name = Tm.named_label "$Size$$" in
          FunEntry {level=Tr.new_level { Tr.parent=Tr.outermost; 
                                         name=name;
                                         formals=[false] } ;
                    label=name;
                    formals=[STRING]; result=INT});

         (Sy.symbol "substring",
          let name = Tm.named_label "$SubString$$" in
          FunEntry {level=Tr.new_level { Tr.parent=Tr.outermost; 
                                         name=name; 
                                         formals=[false;false;false] } ;
                    label=name;
                    formals=[STRING; INT; INT]; result=STRING});

         (Sy.symbol "concat",
          let name = Tm.named_label "$Concat$$" in
          FunEntry {level=Tr.new_level { Tr.parent=Tr.outermost; 
                                         name=name;
                                         formals=[false;false] } ;
                    label=name;
                    formals=[STRING;STRING]; result=STRING});

         (Sy.symbol "not",
          let name = Tm.named_label "$Not$$" in
          FunEntry {level=Tr.new_level { Tr.parent=Tr.outermost; 
                                         name=name;
                                         formals=[false] } ;
                    label=name;
                    formals=[INT]; result=INT});

         (Sy.symbol "exit",
          let name = Tm.named_label "$Exit$$" in
          FunEntry {level=Tr.new_level { Tr.parent=Tr.outermost; 
                                         name=name;
                                         formals=[false] } ;
                    label=name;
                    formals=[INT]; result=UNIT})

      ]
    in
    List.fold_left f Sy.empty entries

end




