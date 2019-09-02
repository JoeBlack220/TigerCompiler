(* Standard ML code courtesy of Gopalan Nadathur.

   Translation to OCaml by Eric Van Wyk. 
 *)

module MipsFrame (Tm: Temp.TEMP)
                 (Ir: Tree.TREE with type temp = Tm.temp
                                 and type label = Tm.label)
                 (Mr: Mipsregs.MIPSREGS with type temp = Ir.temp 
                                         and type temp = Tm.temp)
       : (Frame.FRAME with type label = Tm.label
                       (* and type label = Ir.label*)
                       and type exp = Ir.exp
                       and type stm = Ir.stm
                       and type temp = Tm.temp)
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


  type frag = PROC of {body: Ir.stm list; frame:frame}  (* Changed in A-07 *)
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

  let proc_body _ exp = Ir.MOVE(ret_val, exp) (* Changed in A-07 *)

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
