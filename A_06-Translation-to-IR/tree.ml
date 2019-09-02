(* Code from Appel, extended by Gopalan Nadathur. 
   Translated from Standard ML to OCaml by Eric Van Wyk.
 *)
module type TREE = sig
  type label
  type temp
  type size

  type exp = BINOP of binop * exp * exp
           | MEM of exp
           | TEMP of temp
           | ESEQ of stm * exp
           | NAME of label
           | CONST of int
	   | CALL of exp * exp list

   and binop = PLUS | MINUS | MUL | DIV 
             | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

   and stm = SEQ of stm * stm
           | LABEL of label
           | JUMP of exp * label list
           | CJUMP of relop * exp * exp * label * label
	   | MOVE of exp * exp
           | EXP of exp

   and relop = EQ | NE | LT | GT | LE | GE 
	     | ULT | ULE | UGT | UGE

  val notRel : relop -> relop
end


module Tree (Tm: Temp.TEMP)
       : TREE with type label = Tm.label
               and type temp = Tm.temp = 
struct
  type label = Tm.label
  type size = int
  type temp = Tm.temp

  type exp = BINOP of binop * exp * exp
           | MEM of exp
           | TEMP of Tm.temp
           | ESEQ of stm * exp
           | NAME of label
           | CONST of int
	   | CALL of exp * exp list

   and binop = PLUS | MINUS | MUL | DIV 
             | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

   and stm = SEQ of stm * stm
           | LABEL of label
           | JUMP of exp * label list
           | CJUMP of relop * exp * exp * label * label
	   | MOVE of exp * exp
           | EXP of exp
                  
   and relop = EQ | NE | LT | GT | LE | GE 
	     | ULT | ULE | UGT | UGE

  let notRel = function
    | EQ -> NE
    | NE -> EQ
    | LT -> GE
    | GT -> LE
    | LE -> GT
    | GE -> LT
    | ULT -> UGE    
    | ULE -> UGT
    | UGT -> ULE
    | UGE -> ULT

end

