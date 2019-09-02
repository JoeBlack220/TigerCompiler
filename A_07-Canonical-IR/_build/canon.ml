(* Code courtesy of Gopalan Nadathur. 
   Translted from Standard ML to OCaml by Eric Van Wyk.
 *)

module type CANON = sig
  type label
  type stm

  val canonize : stm -> label -> stm list

  val linearize : stm -> stm list
  (* From an arbitrary Tree statement, produce a list of cleaned trees
     satisfying the following properties:

     1. No SEQ's or ESEQ's
     2. The parent of every CALL is an EXP(..) or a MOVE(TEMP t,..)
   *)

  val basic_blocks : stm list -> label -> stm list list
  (* From a list of cleaned trees and labels for the beginning and 
     exit points for the procedure, produce a list of basic blocks 
     satisfying the following properties:

     1. and 2. as above;
     3. Every block begins with a LABEL;
     4. A LABEL appears only at the beginning of a block;
     5. Any JUMP or CJUMP is the last stm in a block;
     6. Every block ends with a JUMP or CJUMP;

     Notice that the input label will be used to mark the point to 
     which control will be passed upon exit.
   *)

  val trace_schedule : stm list list * label -> stm list
  (* From a list of basic blocks satisfying properties 1-6,
     along with an "exit" label, produce a list of stms such that:

     1. and 2. as above;
     7. Every CJUMP(_,t,f) is immediately followed by LABEL f.

     The blocks are reordered to satisfy property 7; also
     in this reordering as many JUMP(T.NAME(lab)) statements
     as possible are eliminated by falling through into T.LABEL(lab).
   *)
end


module Canon
         (Sy: Symbol.SYMBOL)
         (Tm: Temp.TEMP with type label = Sy.symbol)
         (Ir: Tree.TREE with type label = Sy.symbol
                         and type label = Tm.label
                         and type temp = Tm.temp)
       : (CANON with type label = Sy.symbol
                 and type label = Ir.label
                 and type label = Tm.label
                 and type stm = Ir.stm) = struct

  type label = Ir.label
  type stm = Ir.stm

  let fail n = failwith ("Impossible, check " ^ string_of_int n)

  let rec linearize (stm0: Ir.stm) : Ir.stm list =

    let nop : Ir.stm = Ir.EXP(Ir.CONST 0)
    in

    (* rm_const removes ``nop`` stmts and other ``CONST`` stmts that 
      are also efffectively nop's. 
     *)
    let rm_const (s1: Ir.stm) (s2: Ir.stm) : Ir.stm =
      match s1, s2 with
      | Ir.EXP(Ir.CONST _), x -> x
      | x,  Ir.EXP(Ir.CONST _) -> x
      | x, y -> Ir.SEQ(x,y)
    in

    (* Determine if a stm ``s`` can be put after an exp ``e``
     *)
    let commute (s: Ir.stm) (e: Ir.exp) : bool = match s, e with
      | Ir.EXP(Ir.CONST _), _ -> true
      | _, Ir.NAME _ -> true
      | _, Ir.CONST _ -> true
      | _ -> false
    in
    let rec reorder : Ir.exp list -> (Ir.stm * Ir.exp list) = function 
      | (a::rest) ->
         let (stms,e) = do_exp a 
         in
         let (stms',el) = reorder rest
         in if commute stms' e
	    then (rm_const stms stms', e::el)
	    else let t = Tm.new_temp ()
	         in (rm_const stms (rm_const (Ir.MOVE(Ir.TEMP t, e)) stms'),
                     Ir.TEMP t :: el)
      | [] -> (nop, [])

    and reorder_exp (el: Ir.exp list) (build : Ir.exp list -> Ir.exp)
        : Ir.stm * Ir.exp =
      let (stms, el') = reorder el
      in (stms, build el')

    and reorder_stm (el: Ir.exp list) (build: Ir.exp list -> Ir.stm)
        : Ir.stm = 
      let (stms, el') = reorder el
      in rm_const stms (build el')

    and do_stm : Ir.stm -> Ir.stm = function
      | Ir.SEQ (a, b) -> 
         rm_const (do_stm a) (do_stm b)
        
      | Ir.JUMP (e, labs) -> 
         reorder_stm [e] (function [e'] -> Ir.JUMP(e', labs)
                                 | _ -> fail 1 )

     | Ir.CJUMP (p, a, b, t, f) ->
        reorder_stm [a;b] (function [a;b] -> Ir.CJUMP (p, a, b, t, f)
                                 | _ -> fail 2 )

     | Ir.MOVE (Ir.TEMP t, Ir.CALL (e, el)) ->
        reorder_stm (e::el) 
               (function (e'::el') -> Ir.MOVE (Ir.TEMP t, Ir.CALL (e', el'))
                       | _ -> fail 3 )

     | Ir.MOVE (Ir.TEMP t, Ir.ESEQ (s, e)) ->
        do_stm (Ir.SEQ (s, Ir.MOVE (Ir.TEMP t, e)))

     | Ir.MOVE (Ir.TEMP t, b) ->
        reorder_stm [b] (function [b'] -> Ir.MOVE (Ir.TEMP t, b')
                                | _ -> fail 4 )

     | Ir.MOVE (Ir.MEM e, b) ->
        reorder_stm [e;b] (function [e';b'] -> Ir.MOVE (Ir.MEM e', b')
                                  | _ -> fail 5 )

     | Ir.MOVE (Ir.ESEQ (s, e), b) ->
        do_stm (Ir.SEQ (s, Ir.MOVE (e, b)))

     | Ir.EXP (Ir.CALL(e, el)) ->
        reorder_stm (e::el) (function (e'::el') -> Ir.EXP (Ir.CALL (e', el'))
                                    | _ -> fail 6 )

     | Ir.EXP (Ir.ESEQ (s, ((Ir.CALL _) as e))) ->
        do_stm (Ir.SEQ (s, Ir.EXP e))

     | Ir.EXP e ->
        reorder_stm [e] (function [e'] -> Ir.EXP e'
                                | _ -> fail 7 )

     | s -> reorder_stm [] (function [] -> s
                                   | _ -> fail 8 )

   and do_exp : Ir.exp -> (Ir.stm * Ir.exp) = function
     | Ir.BINOP(p, a, b) ->
        reorder_exp [a;b] (function [a';b'] -> Ir.BINOP (p, a', b')
                                  | _ -> fail 9 )

     | Ir.MEM a ->
        reorder_exp [a] (function [a'] -> Ir.MEM a'
                                | _ -> fail 10 )

     | Ir.ESEQ (s, e) ->
        let stms = do_stm s in
        let (stms', e') = do_exp e in
        (rm_const stms stms', e')

     | (Ir.CALL _) as e ->
        let t = Tm.new_temp ()
        in do_exp (Ir.ESEQ (Ir.MOVE (Ir.TEMP t, e), Ir.TEMP t))

     | e ->
        reorder_exp [] (function [] -> e
                               | _ -> fail 11 )
   in

   (* linear gets rid of the top-level SEQ's, producing a list *)
   let rec linear : (Ir.stm * Ir.stm list) -> Ir.stm list = function 
     | Ir.SEQ(a,b), l -> linear(a,linear(b,l))
     | (s,l) -> s::l

   in (* body of linearize *)
   linear(do_stm stm0, [])


  type block = Ir.stm list


  (* Take list of statements and make basic blocks satisfying
     conditions 3 and 4 above, in addition to the extra condition that
     every block ends with a JUMP or CJUMP 
   *)
  let basic_blocks (stms: Ir.stm list) (dne: label) : block list = 
     let rec blocks = function
       | ([], blist) -> List.rev blist

       | (((Ir.LABEL _) as head) :: tail, blist) ->
          let rec next = function
            | (((Ir.JUMP _) as s) :: rest, this_block) ->
	       end_block (rest, s::this_block)

	    | (((Ir.CJUMP _) as s)::rest, this_block) ->
               end_block (rest, s::this_block)

	    | ((Ir.LABEL lab :: _) as stms, this_block) ->
               next(Ir.JUMP(Ir.NAME lab, [lab]) :: stms, this_block)

	    | (s::rest, this_block) ->
               next (rest, s::this_block)

	    | ([], this_block) ->
	       next ([Ir.JUMP (Ir.NAME dne, [dne])], this_block)
	      
	  and end_block (stms, this_block) = 
	    blocks (stms, List.rev this_block :: blist)
		     
	  in next(tail, [head])

       | (stms, blist) -> blocks (Ir.LABEL (Tm.new_label ()) :: stms, blist)

     in blocks (stms, [])

  let enter_block (b: block) (table: block Sy.table) = match b with
    | (Ir.LABEL s) :: _ -> Sy.enter table s b
    | _ -> table

  let rec split_last : 'a list -> ('a list * 'a) = function
    | [x] -> ([], x)
    | (h::t) -> let (t', last) = split_last t in (h::t', last)
    | _ -> fail 12 

  let rec trace (table: block Sy.table) b rest = 
    match b with
    | Ir.LABEL lab :: _ ->               
       let table = Sy.enter table lab []
       in 
       (match split_last b with
        | (most, Ir.JUMP(Ir.NAME lab, _)) ->
           (match Sy.look table lab with
            | Some ((_::_) as b') -> most @ (trace table b' rest)
	    | _ -> b @ get_next (table, rest)
           )
        | (most,Ir.CJUMP(opr,x,y,t,f)) ->
           (match (Sy.look table t, Sy.look table f) with
            | (_, Some ((_::_) as b')) -> b @ (trace table b' rest)
            | (Some ((_::_) as b'), _) -> 
	       most @ [Ir.CJUMP(Ir.notRel opr,x,y,f,t)]
	       @ (trace table b' rest)
            | _ -> 
               let f' = Tm.new_label ()
	       in most @ [Ir.CJUMP(opr,x,y,t,f');
		          Ir.LABEL f'; Ir.JUMP(Ir.NAME f,[f])]
                  @ 
                    (get_next (table, rest))
           )
        | (most, Ir.JUMP _) -> b @ get_next(table,rest)
        | _ -> fail 13
       )
    | _ -> fail 14

  and get_next : (block Sy.table * block list) -> Ir.stm list = function
    | (table, ((Ir.LABEL lab::_) as b)::rest) ->
       (match Sy.look table lab with
        | Some (_ :: _) -> trace table b rest
        | _ -> get_next(table,rest)
       )
    | (table, []) -> []
    | _ -> fail 15

  let trace_schedule ((blocks: block list), (dne: Ir.label)) 
      : Ir.stm list = 
    let labelled_table : block Sy.table 
      = List.fold_right enter_block blocks Sy.empty 
    in
    get_next (labelled_table, blocks) @ [Ir.LABEL dne]

  let canonize (s: Ir.stm) (dne: label) : Ir.stm list =
      trace_schedule (basic_blocks (linearize s) dne, dne)

end
