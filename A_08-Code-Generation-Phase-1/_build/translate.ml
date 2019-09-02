(* Code courtesy of Gopalan Nadathur. 
   Translted from Standard ML to OCaml by Eric Van Wyk.
 *)

module type TRANSLATE = sig
   type level
   type access
   type label

   val outermost : level
   type level_init = {parent: level; name: label; formals: bool list}
   val new_level : level_init -> level

   (* we assume that the next four functions are never called with 
      outermost frame as argument *)
   val name: level -> label
   val formals: level -> access list
   val parent: level -> level
   val alloc_local: level -> bool -> access
   val get_epilogue: level -> label
   val take_k_values: int -> 'a list -> 'a list
   (* New in A_06 *)

   (* Represents the result of translation. Notice that this is the
      translation, not IR, form of exp as discussed in class. See the
      definition in the functor definition *)
   type exp

   type frag        
   val init_frags: unit -> unit
   val get_results: unit -> (frag list)

   (* Some sample translation functions.  Some of these will
     correspond to Absyn.exp constructors like IntExp, others will
     correspond to Absyn. oper constructos like PlusOp.  
   *)
     
   val intExp: int -> exp
   val plusExp: exp -> exp -> exp
   val minusExp: exp -> exp -> exp
   val timesExp: exp -> exp -> exp
   val divideExp: exp -> exp -> exp
   val eqExp: exp -> exp -> bool -> exp
   val neqExp: exp -> exp -> bool -> exp
   val ltExp: exp -> exp -> bool -> exp
   val leExp: exp -> exp -> bool -> exp
   val gtExp: exp -> exp -> bool -> exp
   val geExp: exp -> exp -> bool -> exp
   val arrayExp: exp -> exp -> level -> exp
   val recExp: exp list -> exp
   val subVarExp: exp -> exp -> level -> exp
   val fieVarExp: exp -> int -> level -> exp
   val ifThenElseExp: exp -> exp -> exp -> exp
   val assignExp: exp -> exp -> exp
   val ifThenExp: exp -> exp -> exp
   val whileExp: exp -> exp -> label -> exp
   val forExp: exp -> bool ref -> exp -> exp -> exp -> label -> exp
   val letExp: exp list -> exp -> exp
   val simpleVar: access -> level -> exp
   val nilExp: unit -> exp
   val seqExp: exp list -> exp
   val string_exp: string -> exp
   val main: level -> exp -> unit
   val callExp: level -> level -> label -> exp list -> exp
   val procEntryExit: level -> exp -> unit
   (* You will need to add your new translations function to the
      TRANSLATE signature as well. 
    *)

end


(* Courtesy of Gopalan Nadathur.
   Minor changes by Eric Van Wyk to avoid nonexhaustive match warnings.
 *)

module Translate 
         (Tm: Temp.TEMP)
         (Ir: Tree.TREE with type label = Tm.label
                         and type temp = Tm.temp)
         (Fr: Frame.FRAME with type label = Tm.label 
                           and type exp = Ir.exp
                           and type stm = Ir.stm
                           and type label = Ir.label)
         (Ca: Canon.CANON with type label = Tm.label
                           and type label = Ir.label
                           and type label = Fr.label
                           and type stm = Ir.stm
                           and type stm = Fr.stm)
       : (TRANSLATE with type label = Tm.label
                     and type frag = Fr.frag ) = struct

  type unique = unit ref

  type level = Bottom 
             | Other of Fr.frame * level * unique 

  type access = level * Fr.access

  type label = Tm.label

  type frag = Fr.frag

  exception UnexpectedCase of string

  let outermost = Bottom

  type level_init = {parent: level; name: label; formals: bool list}

  let new_level (init: level_init) : level =
    Other ( Fr.new_frame {Fr.name = init.name; 
                          formals = true :: init.formals},
            init.parent, ref ()
          )

  let name (l: level) : label = match l with
    | Other (frame, _, _) -> Fr.name frame
    | _ -> raise (UnexpectedCase "in name")

  let parent (l:level) : level = match l with
    | Other (_, parent, _) -> parent
    | _ -> raise (UnexpectedCase "in parent")

  let formals (l: level) : access list = match l with
    | Other (frame, _, _) -> List.map (fun x -> (l, x)) 
                                      (List.tl (Fr.formals frame))
    | _ -> raise (UnexpectedCase "in formals")

  let alloc_local (l: level) (esc: bool) = match l with
    | Other (frame, _, _) -> (l, (Fr.alloc_local frame esc))
    | _ -> raise (UnexpectedCase "in allocLocal")

  let get_epilogue (l:level) : label = match l with
    | Other(frame, _, _) -> Fr.proc_epilogue frame
    | _ -> raise (UnexpectedCase "in name")
  
  let rec take_k_values k xs = match k with
    | 0 -> []
    | k -> match xs with
           | [] -> failwith "take_k_values"
           | y :: ys -> y :: (take_k_values (k - 1) ys)

  (* Fragments 
     --------------------------------------------------
   *)

  (* We collect the fragments created from the program into this list. 
     The code that translates string literals and procedures will add
     new fragments to this list.
   *)
  let fragments: (Fr.frag list) ref = ref []

  (* The fragment list should initially contain at least a label 
     and an associated location to use for nil; compare with this
     when trying to determine if you are dereferencing a null pointer 
   *)
  let nil_label = Tm.new_label ()  
  let unit_label = Tm.new_label ()

  let init_frags () =
      fragments := [Fr.string_frag nil_label " ";
                    Fr.string_frag unit_label " "
                   ]

  (* This function is called from the driver.ml code to retrive the
     generated fragments. 
   *)
  let get_results () = ! fragments


  (* Translation to IR of procedure bodies
     --------------------------------------------------
   *)

  (* The translation form for expressions that was discussed in class.
     This is the value passed back through functions in semant.ml. 
   *)
  type exp = Ex of Ir.exp
           | Nx of Ir.stm
           | Cx of (Tm.label * Tm.label -> Ir.stm)


  (* A useful function for creating a sequence out of a list of 
     two or more statement forms. 
   *)
  let rec seq : Ir.stm list -> Ir.stm = function
    | [s] -> s
    | [s1; s2] -> Ir.SEQ (s1, s2)
    | (s1 :: ss) -> (Ir.SEQ (s1, seq ss))
    | _ -> failwith "unexpected use of seq"

   (* A couple of the coercion functions discussed in class; 
      You should add a definition for mkNx.
     *)
  let mkEx = function 
    | Ex e -> e
    | Cx gen_stm ->
       let r = Tm.new_temp () in
       let t = Tm.new_label () in
       let f = Tm.new_label () in
       Ir.ESEQ(seq [ Ir.MOVE(Ir.TEMP r, Ir.CONST 1);
                     gen_stm (t, f);
                     Ir.LABEL f;
                     Ir.MOVE(Ir.TEMP r, Ir.CONST 0);
                     Ir.LABEL t],
               Ir.TEMP r)
    | Nx s -> Ir.ESEQ(s, Ir.CONST 0)

  let mkCx = function
    | Ex (Ir.CONST 0) ->
       fun (x,y) -> Ir.JUMP (Ir.NAME y, [y])
    | Ex (Ir.CONST 1) ->
       fun (x,y) -> Ir.JUMP (Ir.NAME x, [x])
    | Ex e -> 
       fun (x,y) -> 
       Ir.CJUMP(Ir.EQ, Ir.CONST 0, e, y, x)
    | Cx stm -> stm
    | Nx _ -> failwith "unexpected use of mkCx"
  
  let rec mkNx = function
    | Ex e -> Ir.EXP e
    | Nx s -> s
    | Cx c ->
      let join = Tm.new_label() in
        seq[c(join, join);
            Ir.LABEL join] 


  (* Below are some functions for translating to IR.

     Many constructors in Absyn.exp have corresponding functions below
     that return the intermediate representation code for the Tiger
     expression for the Absyn.exp constructor.

     For example:
     - Absyn.IntExp is translated by the provided function intExp below. 

     - Absyn.SimpleVar may be translated by a function named
     simpleVar : access -> level -> Tr.exp.  You will write this function
     below.

     - You might consider a specific translate function, to write
     below, for each of the aritmetic Abysn.oper constructors.  For
     example, an Absyn.OpExp with oper field of Absyn.PlusOp could be
     translated by a function named plusOp, with the type Tr.exp ->
     Tr.exp -> Tr.exp.

     - For operations like Absyn.EqOp you might have specific
     translation functions for checking integers for equality and
     another for checking strings for equality. Or you may combine them
     into one function as done in the provided function below.

     These are design decisions that you may or may not choose to
     make.  You have a lot of freedom in designing the new functions
     in this Translate module.

     Lastly, you might create a special function for generating bogus
     code for expressions that have type errors and for which you
     cannot generate reasonable IR code.  In these cases, the semantic
     errors reported will prevent the compiler from generating this
     code.  But the return type of our translation functions in
     semant.ml require some value of type Translate.exp be returned,
     so you need some bogus value for that.
   *)

  (* This may be called when translating an integer expression *)  
  let intExp (n: int) : exp = (Ex (Ir.CONST n))
  let ifThenElseExp testExp thenExp elseExp = 
    let t = Tm.new_label() in
    let f = Tm.new_label() in
    let conds() = Cx(fun (t', f') -> seq[ mkCx testExp (t, f);
                                          Ir.LABEL t;
                                          mkCx thenExp (t', f');
                                          Ir.LABEL f;
                                          mkCx elseExp (t', f')])
    in 
    let nores() = 
      let join = Tm.new_label() in
                  Nx(seq [ mkCx testExp (t, f);
                           Ir.LABEL t;
                           mkNx thenExp;
                           Ir.JUMP (Ir.NAME join, [join]);
                           Ir.LABEL f;
                           mkNx elseExp;
                           Ir.LABEL join])
    in
    let exps() = let r = Tm.new_temp() in
        let join = Tm.new_label() in
                 Ex(Ir.ESEQ (seq [ mkCx testExp (t, f);
                                   Ir.LABEL t;
                                   Ir.MOVE (Ir.TEMP r, mkEx thenExp);
                                   Ir.JUMP (Ir.NAME join, [join]);
                                   Ir.LABEL f;
                                   Ir.MOVE (Ir.TEMP r, mkEx elseExp);
                                   Ir.LABEL join], Ir.TEMP r))
    in
    begin match (thenExp, elseExp) with
    | Nx _, _ -> nores()
    | _, Nx _ -> nores()
    | Cx _, _ -> conds()
    | _, Cx _ -> conds()
    | _       -> exps()
    end
  let ifThenExp testExp thenExp = 
    let t = Tm.new_label() in
    let join = Tm.new_label() 
    in
      Nx(seq [ mkCx testExp (t, join);
               Ir.LABEL t;
               mkNx thenExp;
               Ir.LABEL join])

  let whileExp test body done_label =
    let test_label = Tm.new_label() in
    let body_label = Tm.new_label() in
    let test = mkCx test in
    let body = mkNx body in
      Nx(seq[Ir.JUMP(Ir.NAME test_label, [test_label]);
             Ir.LABEL body_label;
             body;
             Ir.LABEL test_label;
             test(body_label, done_label);
             Ir.LABEL done_label])
  let forExp varExp escape loExp hiExp bodyNx breaklabel =
    let var = mkEx varExp in
    let lo = mkEx loExp in
    let hi = mkEx hiExp in
    let body = mkNx bodyNx in
    let bodylabel = Tm.new_label() in
    let updatelabel = Tm.new_label() in
    let hiTm = Tm.new_temp() in
      Nx(seq[Ir.MOVE(var, lo);
             Ir.MOVE((Ir.TEMP hiTm), hi);
             Ir.CJUMP(Ir.LE, var, (Ir.TEMP hiTm), bodylabel, breaklabel);
             Ir.LABEL bodylabel;
             body;
             Ir.CJUMP(Ir.LT, var, (Ir.TEMP hiTm), updatelabel, breaklabel);
             Ir.LABEL updatelabel;
             Ir.MOVE(var, Ir.BINOP(Ir.PLUS, var, Ir.CONST 1));
             Ir.JUMP(Ir.NAME bodylabel, [bodylabel]);
             Ir.LABEL breaklabel])
let seqExp (exps: exp list) = 
    begin match exps with
    | [] -> Nx(Ir.EXP (Ir.NAME unit_label))
    | [x] -> x
    | a :: rest -> 
    let k = (List.length exps) - 1 in
        let lastE = (List.nth exps ((List.length exps) - 1)) in
        let last = (mkEx lastE) in
        let first = (seq (List.map mkNx (take_k_values k exps))) in
        Ex(Ir.ESEQ(first, last))
    end
  
  let letExp decs body = 
    let len = List.length decs in
      if len = 0 then body
      else if len = 1 then Ex(Ir.ESEQ((mkNx (List.hd decs)), (mkEx body)))
      else let s = (List.map mkNx decs) in
        Ex(Ir.ESEQ((seq s), (mkEx body)))

  let simpleVar (acce: access) curlev =
    let (declevel, frameacc) = acce in
    let (decfram, decplevel, decuniq) =
      begin match declevel with
      | Other(tf, tl, tu) -> (tf, tl, tu)
      end
    in
    let rec findFrame = function 
      | Other (curfram, curlevel, curuniq) ->
        if (decuniq == curuniq) then []
        else  curfram :: (findFrame curlevel)
    in
    let framelist = (findFrame curlev) in
    Ex(Fr.gen_Var_Tr framelist frameacc) 
  let assignExp lhs rhs = 
    Nx(Ir.MOVE (mkEx lhs, mkEx rhs))
  let arrayExp size init curLev =
    let doneLabel = (get_epilogue curLev) in
    let sizeTemp = Tm.new_temp() in
    let initTemp = Tm.new_temp() in
    let sizeErrLabel = Tm.new_label() in 
    let fiInitLabel = Tm.new_label() in
      Ex(Ir.ESEQ(seq[Ir.MOVE(Ir.TEMP sizeTemp, (mkEx size));
                     Ir.MOVE(Ir.TEMP initTemp, (mkEx init));
                     Ir.CJUMP(Ir.LT, (Ir.TEMP sizeTemp), Ir.CONST 0, sizeErrLabel, fiInitLabel);
                     Ir.LABEL sizeErrLabel;
                     (mkNx (Ex(Fr.external_call "$SizeError$$" [])));
                     Ir.JUMP(Ir.NAME doneLabel, [doneLabel]);
                     Ir.LABEL fiInitLabel ],
                 Fr.external_call "$InitArray$$" [Ir.TEMP sizeTemp; Ir.TEMP initTemp]
                 ))
  let subVarExp varName varIndex curLev =
    let doneLabel = (get_epilogue curLev) in
    let nameTemp = Tm.new_temp() in
    let indexTemp = Tm.new_temp() in
    let upErrLabel = Tm.new_label() in
    let upOkLabel = Tm.new_label() in
    let loErrLabel = Tm.new_label() in
    let loOkLabel = Tm.new_label() in
      Ex(Ir.MEM(Ir.ESEQ(seq[Ir.MOVE(Ir.TEMP nameTemp, (mkEx varName));
                            Ir.MOVE(Ir.TEMP indexTemp, (mkEx varIndex));
                            Ir.CJUMP(Ir.LE, Ir.MEM (Ir.TEMP nameTemp), Ir.TEMP indexTemp, upErrLabel, upOkLabel);
                          Ir.LABEL upErrLabel;
                          (mkNx (Ex(Fr.external_call "$UpperBoundExit$$" [])));
                            Ir.JUMP(Ir.NAME doneLabel, [doneLabel]);
                            Ir.LABEL upOkLabel;
                            Ir.CJUMP(Ir.LT, Ir.TEMP indexTemp, Ir.CONST 0, loErrLabel, loOkLabel);
                            Ir.LABEL loErrLabel;
                            (mkNx (Ex(Fr.external_call "$LowerBoundExit$$" [])));
                            Ir.JUMP(Ir.NAME doneLabel, [doneLabel]);
                            Ir.LABEL loOkLabel], 
                            Ir.BINOP(Ir.PLUS, Ir.TEMP nameTemp, Ir.BINOP(Ir.MUL, Ir.CONST Fr.ws, Ir.BINOP(Ir.PLUS, Ir.CONST 1, Ir.TEMP indexTemp))))))
  let fieVarExp varName offset curLev =
    let doneLabel = (get_epilogue curLev) in
    let nameTemp = Tm.new_temp() in
    let nullErrLabel = Tm.new_label() in
    let nullOkLabel =  Tm.new_label() in
    let frOffset = Fr.ws * offset in
      Ex(Ir.MEM(Ir.ESEQ(seq[Ir.MOVE(Ir.TEMP nameTemp, (mkEx varName));
                            Ir.CJUMP(Ir.EQ, Ir.TEMP nameTemp, Ir.NAME nil_label, nullErrLabel, nullOkLabel);
                            Ir.LABEL nullErrLabel;
                            (mkNx (Ex(Fr.external_call "$NullExit$$" [])));
                            Ir.JUMP(Ir.NAME doneLabel, [doneLabel]);
                            Ir.LABEL nullOkLabel],
                            Ir.BINOP(Ir.PLUS, Ir.TEMP nameTemp, Ir.CONST frOffset))))
  let recExp exps =
    let varName = Tm.new_temp() in
    let ind = ref 0 in
    let totalOffset = Fr.ws * (List.length exps) in
    let rec genstm = function
      | exp :: rest ->
        let i = !ind * 4 in
          ind := !ind + 1;
          Ir.MOVE(Ir.MEM(Ir.BINOP(Ir.PLUS, Ir.TEMP varName, Ir.CONST i)), (mkEx exp)) :: (genstm rest)
      | [] -> []
    in 
      Ex(Ir.ESEQ(seq ([Ir.MOVE(Ir.TEMP varName, (Fr.external_call "$Malloc$$" [Ir.CONST totalOffset]))] @ (genstm exps)),
                 Ir.TEMP varName))
  let nilExp () = Ex(Ir.NAME nil_label)
  let procEntryExit lev bexp = 
    let levelFrame = 
      begin match lev with
      | Other(fr, _, _) -> fr
      | _ -> failwith "procEntryExit"
      end
    in
    let treeBody = (mkEx bexp) in
    let cannon_stms = Ca.canonize (Fr.proc_body levelFrame treeBody) (Fr.proc_epilogue levelFrame) in
    let new_frag = Fr.proc_frag levelFrame cannon_stms in
      (fragments := new_frag :: (!fragments))
  let callExp callLevel funcLevel label argexps =
    begin match funcLevel with
    | Other(fr, parenLevel, _) -> 
      let eqLevel level1 level2 =
        begin match (level1, level2) with
        | Other(_, _, u1) , Other(_, _, u2) -> u1 == u2
        | Bottom, Bottom -> true
        | _ -> false
        end
    in
      let rec genFrms decLevel curLevel =
        begin match curLevel with
        | Other (fr, upLevel, _ ) ->
          if (eqLevel curLevel decLevel) then  []
          else fr :: (genFrms decLevel upLevel)
        | Bottom -> []
        end
      in
      let staticl =
        let framelist = (genFrms parenLevel callLevel) in
          (Fr.gen_Slink_Tr (List.rev framelist))
      in
      let exArgs = (List.map mkEx argexps) in
        Ex(Ir.CALL (Ir.NAME label, staticl :: exArgs))
    end

  (* This is something to use for translating an addition expression. *)
  let plusExp exp1 exp2 =
      (Ex (Ir.BINOP (Ir.PLUS, (mkEx exp1), (mkEx exp2))))
  let minusExp exp1 exp2 =
      (Ex (Ir.BINOP (Ir.MINUS, (mkEx exp1), (mkEx exp2))))
  let timesExp exp1 exp2 =
      (Ex (Ir.BINOP (Ir.MUL, (mkEx exp1), (mkEx exp2))))
  let divideExp exp1 exp2 =
      (Ex (Ir.BINOP (Ir.DIV, (mkEx exp1), (mkEx exp2))))
  (* Here is something to use when translating equality checking;
     the str parameter indicates whether for string or for integers.
     understand how the Cx form is generated here 
   *)
  let eqExp exp1 exp2 (str: bool) = 
    if str
    then Cx (fun (t,f) -> 
             Ir.CJUMP( Ir.EQ, 
                       (Fr.external_call 
                          "$StringEqual$$" 
                          [ (mkEx exp1); (mkEx exp2) ]),
                       (Ir.CONST 0),
                       f,
                       t )
           )
    else Cx (fun (t,f) -> (Ir.CJUMP (Ir.EQ, mkEx exp1, mkEx exp2, t, f)))

  let neqExp exp1 exp2 (str: bool) = 
    if str
    then Cx (fun (t,f) -> 
             Ir.CJUMP( Ir.NE, 
                       (Fr.external_call 
                          "$StringEqual$$" 
                          [ (mkEx exp1); (mkEx exp2) ]),
                       (Ir.CONST 0),
                       f,
                       t )
           )
    else Cx (fun (t,f) -> (Ir.CJUMP (Ir.NE, mkEx exp1, mkEx exp2, t, f)))

  let leExp exp1 exp2 (str: bool) = 
    if str
    then Cx (fun (t,f) -> 
             Ir.CJUMP( Ir.LE, 
                       (Fr.external_call 
                          "$StringEqual$$" 
                          [ (mkEx exp1); (mkEx exp2) ]),
                       (Ir.CONST 0),
                       f,
                       t )
           )
    else Cx (fun (t,f) -> (Ir.CJUMP (Ir.LE, mkEx exp1, mkEx exp2, t, f)))

  let ltExp exp1 exp2 (str: bool) = 
    if str
    then Cx (fun (t,f) -> 
             Ir.CJUMP( Ir.LT, 
                       (Fr.external_call 
                          "$StringEqual$$" 
                          [ (mkEx exp1); (mkEx exp2) ]),
                       (Ir.CONST 0),
                       f,
                       t )
           )
    else Cx (fun (t,f) -> (Ir.CJUMP (Ir.LT, mkEx exp1, mkEx exp2, t, f)))

  let gtExp exp1 exp2 (str: bool) = 
    if str
    then Cx (fun (t,f) -> 
             Ir.CJUMP( Ir.GT, 
                       (Fr.external_call 
                          "$StringEqual$$" 
                          [ (mkEx exp1); (mkEx exp2) ]),
                       (Ir.CONST 0),
                       f,
                       t )
           )
    else Cx (fun (t,f) -> (Ir.CJUMP (Ir.GT, mkEx exp1, mkEx exp2, t, f)))

  let geExp exp1 exp2 (str: bool) = 
    if str
    then Cx (fun (t,f) -> 
             Ir.CJUMP( Ir.GE, 
                       (Fr.external_call 
                          "$StringEqual$$" 
                          [ (mkEx exp1); (mkEx exp2) ]),
                       (Ir.CONST 0),
                       f,
                       t )
           )
    else Cx (fun (t,f) -> (Ir.CJUMP (Ir.GE, mkEx exp1, mkEx exp2, t, f)))

  (* Here is an example for string literals in which a fragment is created *)
  let string_exp s =
      let lab = Tm.new_label ()
      in (fragments := (Fr.string_frag lab s) :: (!fragments)) ;
         (Ex (Ir.NAME lab))

  (* Here we use 'proc_body' in creating the code for the "main" expression
   *)
  let main (lev: level) (e: exp) : unit = match lev with
    | Other (fr, _, _) ->
       let canon_stms = Ca.canonize (Fr.proc_body fr (mkEx e))
                                    (Fr.proc_epilogue fr) in
       let new_frag = Fr.proc_frag fr canon_stms in
       let _ = fragments := new_frag :: (!fragments) in
       let _ = fragments := List.rev (!fragments)
       in ()
    | Bottom -> failwith "Unexpected level in Tr.main"

end
   
