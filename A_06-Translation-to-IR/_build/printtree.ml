(* Code from Appel.

   Functorized and extended by Gopalan Nadathur. 

   The normalize function and translation to OCaml by Eric Van Wyk. 
 *)

module type PRINTTREE = sig
  type stm
  type frag
  val print_prog: out_channel -> (frag list) -> string -> unit 
  val print_tree: out_channel -> stm -> string -> unit 
  val normalize: (frag list) -> (frag list)
end

module PrintTree (Sy: Symbol.SYMBOL)
                 (Tm: Temp.TEMP with type label = Sy.symbol)
                 (Ir: Tree.TREE with type label = Tm.label
                                 and type label = Sy.symbol
                                 and type temp = Tm.temp)
                 (Fr: Frame.FRAME with type label = Tm.label 
                                   and type exp = Ir.exp
                                   and type stm = Ir.stm
                                   and type label = Ir.label)
       : (PRINTTREE with type stm = Ir.stm
                     and type stm = Fr.stm
                     and type frag = Fr.frag) = struct
  
  type stm = Ir.stm
           
  type frag = Fr.frag

  exception Error

  let print_tree out_chan s0 tempPrefix =
    let say msg = Printf.fprintf out_chan "%s" msg in
    let sayln msg = (say msg; say "\n") in

    let rec indent = function
      | 0 -> ()
      | i -> (say " "; indent(i-1))
    in

    let rec stm s d = match s with
      | Ir.SEQ (a, b) ->
         (indent d; sayln "SEQ("; stm a (d+1); sayln ","; stm b (d+1); 
          say ")"
         )
      | Ir.LABEL lab -> 
         (indent d; say "LABEL "; say (Tm.lab_name lab)
         )
      | Ir.JUMP (e, _) ->  
         (indent d; sayln "JUMP("; exp e (d+1); say ")"
         )
      | Ir.CJUMP(r, a, b, t, f) -> 
         (indent d; say "CJUMP("; relop r; sayln ",";
          exp a (d+1); sayln ","; exp b (d+1); sayln ",";
	  indent (d+1); say(Tm.lab_name t); 
	  say ","; say (Tm.lab_name f); say ")"
         )
      | Ir.MOVE(a, b) ->
         (indent d; sayln "MOVE("; exp a (d+1); sayln ",";
	  exp b (d+1); say ")"
         )
      | Ir.EXP e -> 
         (indent d; sayln "EXP("; exp e (d+1); say ")"
         )

    and exp e d = match e with
      | Ir.BINOP (p, a, b) -> 
         (indent d; say "BINOP("; binop p; sayln ",";
	  exp a (d+1); sayln ","; exp b (d+1); say ")"
         )
      | Ir.MEM e -> 
         (indent d; sayln "MEM("; exp e (d+1); say ")"
         )
      | Ir.TEMP t -> 
         (indent d; say "TEMP "; say tempPrefix; say(Tm.temp_name t)
         )
      | Ir.ESEQ (s, e) -> 
         (indent d; sayln "ESEQ("; stm s (d+1); sayln ","; exp e (d+1); 
          say ")"
         )
      | Ir.NAME lab -> 
         (indent d; say "NAME "; say (Tm.lab_name lab)
         )
      | Ir.CONST i -> 
         (indent d; say "CONST "; say(string_of_int i)
         )
      | Ir.CALL (e, el) -> 
         (indent d; sayln "CALL("; exp e (d+1);
	  List.iter (fun a -> (sayln ","; exp a (d+2))) el; say ")"
         )

    and binop = function
      | Ir.PLUS -> say "PLUS"
      | Ir.MINUS -> say "MINUS"
      | Ir.MUL -> say "MUL"
      | Ir.DIV -> say "DIV"
      | Ir.AND -> say "AND"
      | Ir.OR -> say "OR"
      | Ir.LSHIFT -> say "LSHIFT"
      | Ir.RSHIFT -> say "RSHIFT"
      | Ir.ARSHIFT -> say "ARSHIFT"
      | Ir.XOR -> say "XOR"

    and relop = function 
      | Ir.EQ -> say "EQ"
      | Ir.NE -> say "NE"
      | Ir.LT -> say "LT"
      | Ir.GT -> say "GT"
      | Ir.LE -> say "LE"
      | Ir.GE -> say "GE"
      | Ir.ULT -> say "ULT"
      | Ir.ULE -> say "ULE"
      | Ir.UGT -> say "UGT"
      | Ir.UGE -> say "UGE"

    in  stm s0 0; sayln ""
    (* not needed?  ; TextIO.flushOut outstream *)


  (* translate f s: returns the string generated from s by mapping 
     each character in s by f *)
  let translate f s = 
    let explode (s: string) : char list =
      let l = String.length s
      in
      let rec f i = 
        if i = l then [] else s.[i] :: f (i+1)
      in f 0
    in
    String.concat "" (List.map f (explode s))


  let print_prog out_chan frags tempPrefix =
    let say msg = Printf.fprintf out_chan "%s" msg in
    let sayln msg = (say msg; say "\n") in

    let rec print_frags = function
      | [] -> ()
      | one_frag::rest_frags ->
         let trans_f x = if x = '\"' then "\\\"" else String.make 1 x
         in
         (if (Fr.string_frag_b one_frag) 
          then (say "LABEL ";
	        say ((Fr.string_frag_lab one_frag) ^ "\n");
                say (translate trans_f (Fr.string_frag_str one_frag));
                say "\"\n\n"
               )
	  else (say "LABEL ";
	        sayln (Fr.proc_frag_lab one_frag);
                print_tree out_chan (Fr.proc_frag_body one_frag) tempPrefix;
                sayln ""
               );
	  print_frags rest_frags
         )
    in sayln "The fragments generated from the program: ";
       sayln "";
       print_frags frags
  

  let normalize frags = 
      let lTable : int Sy.table ref = ref Sy.empty in
      let lNew : int ref = ref 0 in
      let tTable : int Sy.table  ref = ref Sy.empty in
      let tNew : int ref = ref 0 in

      let lUpdate k = 
        (if String.sub (Sy.name k) 0 1 = "$" 
         then ()
         else match Sy.look (!lTable) k with
             | Some _ -> ()
             | None -> let v = !lNew in
                       let new_table = Sy.enter (!lTable) k v in
                       lNew := !lNew + 1;
                       lTable := new_table
        ) in
      let lNewLabel k
        = if String.sub (Sy.name k) 0 1 = "$" 
          then k
          else match Sy.look (!lTable) k with
               | None -> failwith ("Error: Tiger Compiler Bug: in lLook")
               | Some n -> Tm.named_label ("N" ^ string_of_int n)
      in
      let tUpdate k = match Sy.look (!tTable) k with
        | Some _ -> ()
        | None -> let v = !tNew in
                  let new_table = Sy.enter (!tTable) k v in
                  tNew := !tNew + 1;
                  tTable := new_table
      in
      let tNewTemp t = 
        match Sy.look (!tTable) t with
        | None -> failwith ("Error: Tiger Compiler Bug: in lLook")
        | Some n -> Tm.named_temp n
      in
      let rec findStm = function
        | Ir.SEQ(a, b) -> (findStm a; findStm b)
        | Ir.LABEL lab -> lUpdate lab
        | Ir.JUMP (e, ls) -> (findExp e; List.iter lUpdate ls)
        | Ir.CJUMP (_, l, r, t, f) ->
            (findExp l; findExp r; lUpdate t; lUpdate f)
        | Ir.MOVE (l, r) -> (findExp l; findExp r)
        | Ir.EXP e -> findExp e
      and findExp = function
        | Ir.BINOP(_, l, r) -> (findExp l; findExp r)
        | Ir.MEM(e) -> findExp e
        | Ir.TEMP t -> tUpdate (Sy.symbol (Tm.temp_name t))
        | Ir.ESEQ(s,e) -> (findStm s; findExp e)
        | Ir.NAME lab -> lUpdate lab
        | Ir.CONST _ -> ()
        | Ir.CALL (f, es) -> List.iter findExp (f::es)
      in
      let findProcFrag frag =
        let oldLabel = Fr.proc_frag_lab' frag in
        let oldBody = Fr.proc_frag_body frag in
        lUpdate oldLabel;
        findStm oldBody
      in
      let rec findFrags = function
        | [] -> ()
        | frag::rest ->
           if (Fr.string_frag_b frag) 
           then lUpdate (Fr.string_frag_lab' frag)
           else findProcFrag frag;
           findFrags rest
      in
      let rec fixStm = function
        | Ir.SEQ(a,b) -> Ir.SEQ (fixStm a, fixStm b)
        | Ir.LABEL lab -> Ir.LABEL (lNewLabel lab)
        | Ir.JUMP (e, ls) -> Ir.JUMP (fixExp e, List.map lNewLabel ls)
        | Ir.CJUMP (relop, l, r, t, f) ->
           Ir.CJUMP (relop, fixExp l, fixExp r, lNewLabel t, lNewLabel f)
        | Ir.MOVE (l, r) -> Ir.MOVE (fixExp l, fixExp r)
        | Ir.EXP e -> Ir.EXP (fixExp e)

      and fixExp = function
        | Ir.BINOP(binop,l,r) -> Ir.BINOP (binop, fixExp l, fixExp r)
        | Ir.MEM(e) -> Ir.MEM (fixExp e)
        | Ir.TEMP x -> Ir.TEMP (tNewTemp (Sy.symbol (Tm.temp_name x)))
        | Ir.ESEQ(s,e) -> Ir.ESEQ(fixStm s, fixExp e)
        | Ir.NAME n -> Ir.NAME (lNewLabel n)
        | Ir.CONST c -> Ir.CONST c
        | Ir.CALL (f, es) -> Ir.CALL (fixExp f, List.map fixExp es)
      in
      let fixProcFrag frag
        = let oldLabel = Fr.proc_frag_lab' frag in
          let newLabel = lNewLabel oldLabel in
          let bogusFrame = Fr.new_frame {name = newLabel ; Fr.formals = []}
          (* of course, this is where the normalization process is
             incomplete and why we can only use this for printing
             purposes *)
          in
          let oldBody = Fr.proc_frag_body frag in
          let newBody = fixStm oldBody
          in
            Fr.proc_frag bogusFrame newBody
      in
      let fixStringFrag frag
        = let oldLabel = Fr.string_frag_lab' frag in
          let newLabel = lNewLabel oldLabel in
          let str = Fr.string_frag_str frag
          in 
            Fr.string_frag newLabel str
      in
      let rec fixFrags = function
        | [] -> []
        | (frag::rest) ->
           (if (Fr.string_frag_b frag) 
            then fixStringFrag frag
            else fixProcFrag frag) 
           :: 
           (fixFrags rest)
      in
      (findFrags frags;
       fixFrags frags)

end

