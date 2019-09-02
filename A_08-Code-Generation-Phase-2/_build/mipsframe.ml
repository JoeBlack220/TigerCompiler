(* Standard ML code courtesy of Gopalan Nadathur.

   Translation to OCaml by Eric Van Wyk. 
 *)

module MipsFrame (Tm: Temp.TEMP)
                 (Ir: Tree.TREE with type temp = Tm.temp
                                 and type label = Tm.label)
                 (As: Assem.ASSEM with type temp = Tm.temp
                                   and type temp = Ir.temp
                                   and type label = Tm.label
                                   and type label = Ir.label)
                 (Mr: Mipsregs.MIPSREGS with type temp = Ir.temp 
                                         and type temp = Tm.temp
                                         and type temp = As.temp)
       : (Frame.FRAME with type label = Tm.label
                       (* and type label = Ir.label*)
                       and type exp = Ir.exp
                       and type stm = Ir.stm
                       and type temp = Tm.temp
                       and type instr = As.instr)
  = struct

  type label = Tm.label

  type access = InFrame of int | InReg of Tm.temp

  type frame = { label: Tm.label;
                 epilogue : label;
                 num_formals : int;
                 formals : access list;
                 offset: int ref;
                 fsize: int ref
               }

  type frame_init = {name: label; formals: bool list}

  type exp = Ir.exp
  type stm = Ir.stm
  type temp = Tm.temp

  (* multiplier for words *)
  let ws = 4
  let num_args_regs = 4

  (* names of some register needed in frame related activity *) 
  let rv_reg = Mr.rv_reg
  let fp_reg = Mr.fp_reg
  let ret_val = (Ir.TEMP rv_reg)

  let new_frame (init: frame_init) : frame =
    let num_args = List.length init.formals 
    in { label = init.name;
         epilogue = Tm.new_label ();
         num_formals = num_args;
         formals = (let i = ref (0 - num_args) in
                    let last_arg_reg = !i + num_args_regs in
                    let f = function
                      | true -> (i := (!i) + 1;
                                 InFrame(ws * (!i)))
                      | false ->  (i := (!i) + 1;
                                   if (!i > last_arg_reg)
                                   then InFrame (ws * (!i))
                                   else InReg (Tm.new_temp()) ) 
                    in List.map f init.formals) ;
         offset = ref (0 - (ws * num_args));
         fsize = ref (0 - (ws * num_args))
       }

  let name ({label;epilogue;num_formals;formals;offset;fsize}:frame) = label

  let formals ({label;epilogue;num_formals;formals;offset;fsize}:frame) = formals

  let alloc_local (f: frame) (b: bool) : access =
    match b with
    | true -> (f.offset := ! (f.offset) - ws; 
               f.fsize := (!(f.fsize)) - ws; 
               InFrame (!(f.offset)))
    | false -> InReg (Tm.new_temp() )


  type frag = PROC of {body: Ir.stm list; frame:frame}  (* A7 *)
            | STRING of label * string

  let string_frag lab str = STRING (lab,str)

  let proc_frag fr stms = PROC {body = stms; frame = fr}

  let string_frag_b = function
    | STRING _ -> true
    | PROC _ -> false

  let string_frag_lab = function
    | STRING (lab, _) -> Tm.lab_name lab
    | _ -> failwith "Error: Tiger Compiler bug: bad call to string_frag_lab"

  let string_frag_lab' = function
    | STRING (lab, _) -> lab
    | _ -> failwith "Error: Tiger Compiler bug: bad call to string_frag_lab'"

  let string_frag_str = function
    | STRING (_, str) -> str
    | _ -> failwith "Error: Tiger Compiler bug: bad call to string_frag_str"

  let proc_frag_lab = function 
    | PROC {body; frame} -> Tm.lab_name (name frame)
    | _ -> failwith "Error: Tiger Compiler bug: bad call to proc_frag_lab"

  let proc_frag_lab' = function
    | PROC {body; frame} -> name frame
    | _ -> failwith "Error: Tiger Compiler bug: bad call to proc_frag_lab'"

  let proc_frag_body = function
    | PROC {body; frame} -> body
    | _ -> failwith "Error: Tiger Compiler bug: bad call to proc_frag_body"


  let external_call str exps =
        Ir.CALL (Ir.NAME(Tm.named_label str), exps)

  (* New functions for A8 *)
  type instr = As.instr

  let procFragFrame = function
    | PROC {body;frame} -> frame
    | _ -> failwith "procFragFrame failing"

  let procPrologue ({label=lab}:frame) = lab

  (* dummy reg for stashing away the return address reg *)
  let dummyRAReg = Tm.new_temp ()

  (* dummy regs for stashing away callee save registers *)
  let stashCalleeRegs =
    let rec mkReg = function
      | 0 -> []
      | n -> (Tm.new_temp()) :: (mkReg (n-1))
    in Array.of_list (mkReg (Array.length (Mr.callee_save_regs)))


  (* adding the prologue and epilogue parts to the procedure body *)
  let proc_body_phase2 ({formals=fl}:frame) exp =
    let rec mapargs xs i = match xs with
      | [] -> []
      | ((InFrame(loc))::fl) ->
         if (i < Mr.numargsregs)
         then (Ir.MOVE(
                   Ir.MEM(Ir.BINOP(Ir.PLUS,
                                   Ir.CONST loc,Ir.TEMP fp_reg)),
                   Ir.TEMP (Array.get (Mr.argregs) i)))
              ::
                mapargs fl (i+1)
         else []
      | ((InReg(t))::fl) ->
         if (i < Mr.numargsregs)
         then (Ir.MOVE(Ir.TEMP t,
                       Ir.TEMP (Array.get (Mr.argregs) i))) 
              ::
                mapargs fl (i+1)
         else []
    in
    let moveargs = mapargs fl 0
    in
    let rec calleeMoves ra1 ra2 i moves = 
      if (i < 0) then moves
      else calleeMoves ra1 ra2 (i-1) 
                       ((Ir.MOVE(
                             Ir.TEMP(Array.get ra1 i), 
                             Ir.TEMP(Array.get ra2 i)))::moves)
    in
    let stashCallees = calleeMoves stashCalleeRegs 
                                   Mr.callee_save_regs 
                                   ((Array.length (Mr.callee_save_regs)) - 1)
                                   []
    in
    let unstashCallees = calleeMoves Mr.callee_save_regs 
                                     stashCalleeRegs
                                     ((Array.length(
                                           Mr.callee_save_regs)) - 1)
                                     []
    in
    let stashRAReg = [Ir.MOVE(Ir.TEMP dummyRAReg,
                              Ir.TEMP Mr.ra_reg)]
    in
    let unstashRAReg = [Ir.MOVE(Ir.TEMP Mr.ra_reg,
                                Ir.TEMP dummyRAReg)]
    in
    let body = [Ir.MOVE(ret_val,exp)]
    in
    let rec seq = function
      | [s] -> s
      | (s::sl) -> Ir.SEQ(s,seq sl)
      | _ -> failwith "in seq in proc_body_phase2"
    in
    seq (moveargs @ stashCallees @ stashRAReg 
         @ body @ unstashCallees @ unstashRAReg)


  
  (* Finishing the code for the procedure with the "view shift" stuff,
     called from regalloc.ml *)
  let entryExit ({num_formals = nf; fsize = fs}:frame) il =

    (* when entering a function ... *)
    [
      (* sw $FP, -( (nf+1)*WS ) ($SP),
         save calling function $FP just past static link, in that 
         "blank" space *) 
      As.OPER {assem = "sw `s0,-" ^ (string_of_int ((nf+1) * ws)) ^
                         "(`s1)\n";
               dst = []; src = [fp_reg; Mr.sp_reg]; jump = None};

      (* la $FP, -4($SP),
         $FP is set up, just below the previous $SP *)
      As.OPER {assem = "la `d0,-" ^ (string_of_int ws) ^ "(`s0)\n";
               dst = [fp_reg]; src = [Mr.sp_reg]; jump = None};

      (* la $SP, fsize($FP),
         $SP is set up, size of frame below new $FP *)
      As.OPER {assem = "la `d0," ^ (string_of_int (!fs)) ^ "(`s0)\n";
               dst = [Mr.sp_reg]; src = [fp_reg]; jump = None}
    ]  @

    il @
 
    (* cleaning up after a function ... *)
    [
      (* la $SP 4($FP),
         restore $SP, 4 above the unrestored $FP *)
      As.OPER {assem = "la `d0," ^ (string_of_int ws) ^ "(`s0)\n";
               dst = [Mr.sp_reg]; src = [fp_reg]; jump = None};

      (* lw $FP, -((nf+1)*ws) $SP, 
         restore $FP, from newly restored $SP *)
      As.OPER {assem = "lw `d0,-" ^ (string_of_int ((nf+1) * ws)) ^
                         "(`s0)\n";
               dst = [fp_reg]; src = [Mr.sp_reg]; jump = None};

      (* jr $RA,   return, jump to address in return address register *)
      As.OPER {assem = "jr `s0\n";
               dst = []; src = [Mr.ra_reg]; jump = None}
    ]

  (* Spilling a temporary onto stack; only tricky part is that if it 
     is a register space is already reserved for it.
     Input: list of temps, output: list of temp, InFrame access pairs *)
  let spillTemps (({formals=fl;num_formals=nf} as frame):frame) tl =
    let rec checkinFormals t xs i = match xs with
      | [] -> None
      | (InReg t') :: fls ->
         if (t = t') then Some (InFrame (0-((nf-i)*ws)))
         else checkinFormals t fls (i+1)
      | _::fls -> checkinFormals t fls (i+1)
    in
    let spillTemp t = 
      let isformal = checkinFormals t fl 1
      in match isformal with
         | None -> alloc_local frame true
         | Some v -> v
    in
    let rec spillTemps_aux = function
      | [] -> []
      | t::tl -> (t,spillTemp t) :: spillTemps_aux tl

    in spillTemps_aux tl

  let loadSpill (t, acc) = match acc with 
    | InFrame i ->
       As.OPER {assem="lw `d0," ^ (string_of_int i) ^ "(`s0)\n";
                src=[fp_reg]; dst=[t]; jump = None}
    | _ -> failwith "Error: attempt to load spilled register access"

  let storeSpill (t, acc) = match acc with
    |InFrame i ->
      As.OPER {assem="sw `s0," ^ (string_of_int i) ^ "(`s1)\n";
               src=[t;fp_reg]; dst=[]; jump = None}
    | _ -> failwith "Error: attempt to store spilled register access"

  let proc_body_phase1 _ exp = Ir.MOVE(ret_val, exp)



  (* CHANGE THIS WHEN MOVING TO PHASE 2 *)
  let proc_body =(*  proc_body_phase1 *)
                  proc_body_phase2


  let proc_epilogue ({epilogue=lab}:frame) = lab

  let rec getExp (framelist: frame list) =
    begin match framelist with
    | [] ->
      (Ir.TEMP fp_reg)
    | fr :: rest -> 
      (Ir.MEM (Ir.BINOP (Ir.PLUS, (Ir.CONST (!(fr.offset) + ws)), (getExp rest))))
    end

  let rec gen_Var_Tr2 (framelist: frame list) = 
    begin match framelist with
    | [] -> (Ir.TEMP fp_reg)
    | fr :: rest -> 
      begin match (List.hd fr.formals) with
      | InReg temp -> (Ir.TEMP temp)
      | InFrame k -> 
        (Ir.MEM (Ir.BINOP (Ir.PLUS, Ir.CONST k, (gen_Var_Tr2 rest))))
      end
    end

  let gen_Var_Tr (framelist: frame list) acc =
    begin match acc with
    | InReg temp -> (Ir.TEMP temp)
    | InFrame k ->
      (Ir.MEM (Ir.BINOP (Ir.PLUS, Ir.CONST k, (gen_Var_Tr2 (List.rev framelist)))))
    end

  let rec gen_Slink_Tr (framelist: frame list) =
    begin match framelist with
    | [] ->
      (Ir.TEMP fp_reg)
    | fr :: rest -> 
      (gen_Var_Tr2 framelist)
    end
  let gen_mulLink fr = 
    Ir.MEM(Ir.BINOP(Ir.PLUS, Ir.CONST (!(fr.offset) + ws), (Ir.TEMP fp_reg)))
end
