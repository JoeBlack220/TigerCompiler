(* Code courtesy of Gopalan Nadathur. 
   Translted from Standard ML to OCaml by Eric Van Wyk.
 *)

module type MIPSCODEGEN = sig
  type stm
  type instr

  val codegen : stm list -> instr list
end

module MipsCodeGen (Ir: Tree.TREE)
                   (As: Assem.ASSEM with type label = Ir.label
                                     and type temp = Ir.temp)
                   (Tm: Temp.TEMP with type label = Ir.label
                                   and type label = As.label
                                   and type temp = Ir.temp
                                   and type temp = As.temp)
                   (Mr: Mipsregs.MIPSREGS with type temp = Ir.temp
                                           and type temp = As.temp
                                           and type temp = Tm.temp)
                   (Fr: Frame.FRAME)
       : (MIPSCODEGEN with type stm = Ir.stm
                       and type instr = As.instr) = struct

  type stm = Ir.stm
  type instr = As.instr

  exception Error

  let incomplete _ = print_endline "Incomplete code in mipscodegen.ml";
                     raise Error

  let codegen (stml : stm list) : instr list =

    let to_unit _ = ()
    in
    let ilist = ref ([]: instr list)
    in
    let emit x = ilist := x :: !ilist
    in
    let rec munchStmList = function
      | [] -> ()
      | s::sl -> munchStm s; munchStmList sl

    (* Some cases are filled in to show how to proceed; you need to
       fill in others in place of the clauses that raise exceptions to
       get complete coverage.  Make sure to understand the role of
       destinations, sources and jumps before starting. Also pay
       attention to using MOVE for register to regiCJUMPster transfer; this
       will permit their elimination where possible later 
     *)

    and munchStm = function
      | Ir.MOVE (dst, src) -> munchMove dst src

      | Ir.LABEL l ->
         emit (As.LABEL {assem = Tm.lab_name l ^ ":\n";
                         lab = l})

      | Ir.JUMP (Ir.NAME l, _) ->
         emit (As.OPER {assem = "j `j0\n";
                        dst = []; src = []; jump = Some [l]})

      | Ir.JUMP (e, ll) ->
         emit (As.OPER {assem = "jr `s0\n";
                        dst = []; src = [munchExp e];
                        jump = Some ll})

      | Ir.EXP e -> to_unit (munchExp e)  ; ()

      | Ir.SEQ (_,_) -> 
         (print_endline ("SEQ seen in code generation, " ^ 
                         "this should not be possible.");
          raise Error)

      | Ir.CJUMP (relop, e1, e2, l1, l2) ->
         (munchCjump relop e1 e2 l1 l2)

      | _ -> incomplete ()  (* Add code here *)

    and munchCjump relop e1 e2 l1 l2 = 
       match relop, e1, e2 with
       | (Ir.EQ, Ir.CONST i1, Ir.CONST i2) ->
         if (i1 = i2) then 
            emit (As.OPER {assem = "j `j0\n";
                           dst = []; src = [];
                           jump = Some [l1]})
         else  
            emit (As.OPER {assem = "beq `s0," ^ (string_of_int i1) ^ ",`j0\n";
                           dst = []; src = [munchExp e2];
                           jump = Some [l1;l2]}) 
       | (Ir.EQ, e1, Ir.CONST 0) ->
            emit (As.OPER {assem = "beqz `s0,`j0\n";
                           dst = []; src = [munchExp e1];
                           jump = Some [l1;l2]})
       | (Ir.EQ, e1, Ir.CONST c) ->
            emit (As.OPER {assem = "beq `s0," ^ (string_of_int c) ^ ",`j0\n";
                           dst = []; src = [munchExp e1];
                           jump = Some [l1;l2]})                                
       | (Ir.EQ, e1, e2) ->
            emit (As.OPER {assem = "beq `s0,`s1,`j0\n";
                           dst = []; src = [munchExp e1; munchExp e2];
                           jump = Some [l1;l2]})              
       | (Ir.NE, Ir.CONST i1, Ir.CONST i2) ->
         if (i1 <> i2) then 
            emit (As.OPER {assem = "j `j0\n";
                           dst = []; src = [];
                           jump = Some [l1]})
         else  
            emit (As.OPER {assem = "bne `s0," ^ (string_of_int i1) ^ ",`j0\n";
                           dst = []; src = [munchExp e2];
                           jump = Some [l1;l2]})
       | (Ir.NE, e1, Ir.CONST 0) ->
            emit (As.OPER {assem = "bnez `s0,`j0\n";
                           dst = []; src = [munchExp e1];
                           jump = Some [l1;l2]})  
       | (Ir.NE, e1, Ir.CONST c) ->
            emit (As.OPER {assem = "bne `s0," ^ (string_of_int c) ^ "`j0\n";
                           dst = []; src = [munchExp e1];
                           jump = Some [l1;l2]})                              
       | (Ir.NE, e1, e2) ->
            emit (As.OPER {assem = "bne `s0,`s1,`j0\n";
                           dst = []; src = [munchExp e1; munchExp e2];
                           jump = Some [l1;l2]})                                      
       | (Ir.LT, Ir.CONST i1, Ir.CONST i2) ->
         if (i1 < i2) then 
            emit (As.OPER {assem = "j `j0\n";
                           dst = []; src = [];
                           jump = Some [l1]})
         else  
            emit (As.OPER {assem = "bgt `s0," ^ (string_of_int i1) ^ ",`j0\n";
                           dst = []; src = [munchExp e2];
                           jump = Some [l1;l2]}) 
       | (Ir.LT, e1, Ir.CONST 0) ->
            emit (As.OPER {assem = "bltz `s0,`j0\n";
                           dst = []; src = [munchExp e1];
                           jump = Some [l1;l2]})
       | (Ir.LT, e1, Ir.CONST i2) ->
            emit (As.OPER {assem = "blt `s0," ^ (string_of_int i2) ^ ",`j0\n";
                           dst = []; src = [munchExp e1];
                           jump = Some [l1;l2]})                                
       | (Ir.LT, e1, e2) ->
            emit (As.OPER {assem = "blt `s0,`s1,`j0\n";
                           dst = []; src = [munchExp e1; munchExp e2];
                           jump = Some [l1;l2]})  
       | (Ir.GT, Ir.CONST i1, Ir.CONST i2) ->
         if (i1 > i2) then 
            emit (As.OPER {assem = "j `j0\n";
                           dst = []; src = [];
                           jump = Some [l1]})
         else  
            emit (As.OPER {assem = "blt `s0," ^ (string_of_int i1) ^ ",`j0\n";
                           dst = []; src = [munchExp e2];
                           jump = Some [l1;l2]}) 
       | (Ir.GT, e1, Ir.CONST 0) ->
            emit (As.OPER {assem = "bgtz `s0,`j0\n";
                           dst = []; src = [munchExp e1];
                           jump = Some [l1;l2]})
       | (Ir.GT, e1, Ir.CONST i2) ->
            emit (As.OPER {assem = "bgt `s0," ^ (string_of_int i2) ^ ",`j0\n";
                           dst = []; src = [munchExp e1];
                           jump = Some [l1;l2]})                              
       | (Ir.GT, e1, e2) ->
            emit (As.OPER {assem = "bgt `s0,`s1,`j0\n";
                           dst = []; src = [munchExp e1; munchExp e2];
                           jump = Some [l1;l2]})                                          
       | (Ir.LE, Ir.CONST i1, Ir.CONST i2) ->
         if (i1 <= i2) then 
            emit (As.OPER {assem = "j `j0\n";
                           dst = []; src = [];
                           jump = Some [l1]})
         else  
            emit (As.OPER {assem = "bge `s0," ^ (string_of_int i1) ^ ",`j0\n";
                           dst = []; src = [munchExp e2];
                           jump = Some [l1;l2]}) 
       | (Ir.LE, e1, Ir.CONST 0) ->
            emit (As.OPER {assem = "blez `s0,`j0\n";
                           dst = []; src = [munchExp e1];
                           jump = Some [l1;l2]})  
       | (Ir.LE, e1, Ir.CONST c) ->
            emit (As.OPER {assem = "ble `s0," ^ (string_of_int c) ^ "`j0\n";
                           dst = []; src = [munchExp e1];
                           jump = Some [l1;l2]})                               
       | (Ir.LE, e1, e2) ->
            emit (As.OPER {assem = "ble `s0,`s1,`j0\n";
                           dst = []; src = [munchExp e1; munchExp e2];
                           jump = Some [l1;l2]})                                      
       | (Ir.GE, Ir.CONST i1, Ir.CONST i2) ->
         if (i1 >= i2) then 
            emit (As.OPER {assem = "j `j0\n";
                           dst = []; src = [];
                           jump = Some [l1]})
         else  
            emit (As.OPER {assem = "ble `s0," ^ (string_of_int i1) ^ ",`j0\n";
                           dst = []; src = [munchExp e2];
                           jump = Some [l1;l2]}) 
       | (Ir.GE, e1, Ir.CONST 0) ->
            emit (As.OPER {assem = "bgez `s0,`j0\n";
                           dst = []; src = [munchExp e1];
                           jump = Some [l1;l2]})  
       | (Ir.GE, e1, Ir.CONST c) ->
            emit (As.OPER {assem = "bge `s0," ^ (string_of_int c) ^ "`j0\n";
                           dst = []; src = [munchExp e1];
                           jump = Some [l1;l2]})                                        
       | (Ir.GE, e1, e2) ->
            emit (As.OPER {assem = "bge `s0,`s1,`j0\n";
                           dst = []; src = [munchExp e1; munchExp e2];
                           jump = Some [l1;l2]})  
       | (Ir.ULT, e1, e2) ->
            emit (As.OPER {assem = "bltu `s0,`s1,`j0\n";
                           dst = []; src = [munchExp e1; munchExp e2];
                           jump = Some [l1;l2]})  
       | (Ir.ULE, e1, e2) ->
            emit (As.OPER {assem = "bleu `s0,`s1,`j0\n";
                           dst = []; src = [munchExp e1; munchExp e2];
                           jump = Some [l1;l2]})  
       | (Ir.UGT, e1, e2) ->
            emit (As.OPER {assem = "bgtu `s0,`s1,`j0\n";
                           dst = []; src = [munchExp e1; munchExp e2];
                           jump = Some [l1;l2]})  
       | (Ir.UGE, e1, e2) ->
            emit (As.OPER {assem = "bgeu `s0,`s1,`j0\n";
                           dst = []; src = [munchExp e1; munchExp e2];
                           jump = Some [l1;l2]})                                                              
    and munchMove dst src = match dst, src with
      | (Ir.TEMP t, Ir.CONST c) ->
         emit (As.OPER {assem = "li `d0," ^ (string_of_int c) ^ "\n";
                        dst = [t]; src = []; jump = None})
      | (Ir.TEMP t, Ir.NAME l) ->
         emit (As.OPER {assem = "la `d0," ^ (Tm.lab_name l) ^ "\n";
                       dst = [t]; src = []; jump = None})       

      | (Ir.TEMP t1, Ir.MEM(Ir.BINOP(Ir.PLUS, e2, Ir.CONST c))) ->
         emit (As.OPER {assem = "lw `d0," ^ (string_of_int c) ^ "(`s0)\n";
                       dst = [t1]; src = [munchExp e2]; jump = None})  
(*       | (Ir.TEMP t1, Ir.MEM(Ir.BINOP(Ir.PLUS, Ir.CONST c, Ir.TEMP t2))) ->
         emit (As.OPER {assem = "lw `d0," ^ (string_of_int c) ^ "(`s0)\n";
                       dst = [t1]; src = [t2]; jump = None}) *)
      | (Ir.TEMP t1, Ir.MEM(Ir.BINOP(Ir.PLUS, Ir.CONST c, e2))) ->
         emit (As.OPER {assem = "lw `d0," ^ (string_of_int c) ^ "(`s0)\n";
                       dst = [t1]; src = [munchExp e2]; jump = None})  
      | (Ir.TEMP t1, Ir.MEM(Ir.BINOP(Ir.MINUS, e2, Ir.CONST c))) ->
         emit (As.OPER {assem = "lw `d0,-" ^ (string_of_int c) ^ "(`s0)\n";
                       dst = [t1]; src = [munchExp e2]; jump = None})  
(*       | (Ir.TEMP t1, Ir.MEM(Ir.BINOP(Ir.MINUS, Ir.CONST c, Ir.TEMP t2))) ->
         emit (As.OPER {assem = "lw `d0,-" ^ (string_of_int c) ^ "(`s0)\n";
                       dst = [t1]; src = [t2]; jump = None}) *)
      | (Ir.TEMP t1, Ir.MEM(Ir.BINOP(Ir.MINUS, Ir.CONST c, e2))) ->
         emit (As.OPER {assem = "lw `d0,-" ^ (string_of_int c) ^ "(`s0)\n";
                       dst = [t1]; src = [munchExp e2]; jump = None})   

      | (Ir.TEMP t1, Ir.MEM e1) ->
         emit (As.OPER {assem = "lw `d0,(`s0)\n";
                       dst = [t1]; src = [munchExp e1]; jump = None})  
(*       | (Ir.MEM(Ir.BINOP(Ir.PLUS, Ir.CONST c, e1)), e2) ->
         emit (As.OPER {assem = "sw `s0," ^ (string_of_int c)^ "(`d0)\n";
                       dst = [munchExp e1]; src = [munchExp e2]; jump = None})          
      | (Ir.MEM(Ir.BINOP(Ir.PLUS, e1, Ir.CONST c)), e2) ->
         emit (As.OPER {assem = "sw `s0," ^ (string_of_int c)^ "(`d0)\n";
                       dst = [munchExp e1]; src = [munchExp e2]; jump = None})  
      | (Ir.MEM e1, e2) ->
         emit (As.OPER {assem = "sw `s0,(`d0)\n";
                       dst = [munchExp e1]; src = [munchExp e2]; jump = None}) 

      | (Ir.MEM(Ir.BINOP(Ir.PLUS, Ir.TEMP t1, Ir.CONST c)), e2) ->
         emit (As.OPER {assem = "sw `s0," ^ (string_of_int c)^ "(`d0)\n";
                       dst = [t1]; src = [munchExp e2]; jump = None}) *)
      | (Ir.MEM(Ir.BINOP(Ir.PLUS, Ir.CONST c, e1)), e2) ->
         emit (As.OPER {assem = "sw `s1," ^ (string_of_int c)^ "(`s0)\n";
                       dst = []; src = [munchExp e1;munchExp e2]; jump = None})          
      | (Ir.MEM(Ir.BINOP(Ir.PLUS, e1, Ir.CONST c)), e2) ->
         emit (As.OPER {assem = "sw `s1," ^ (string_of_int c)^ "(`s0)\n";
                       dst = []; src = [munchExp e1;munchExp e2]; jump = None})  
      | (Ir.MEM(Ir.BINOP(Ir.MINUS, Ir.CONST c, e1)), e2) ->
         emit (As.OPER {assem = "sw `s1,-" ^ (string_of_int c)^ "(`s0)\n";
                       dst = []; src = [munchExp e1;munchExp e2]; jump = None})          
      | (Ir.MEM(Ir.BINOP(Ir.MINUS, e1, Ir.CONST c)), e2) ->
         emit (As.OPER {assem = "sw `s1,-" ^ (string_of_int c)^ "(`s0)\n";
                       dst = []; src = [munchExp e1;munchExp e2]; jump = None})           

      | (Ir.MEM(Ir.BINOP(Ir.PLUS, Ir.TEMP t1, Ir.CONST c)), e2) ->
         emit (As.OPER {assem = "sw `s1," ^ (string_of_int c)^ "(`s0)\n";
                       dst = []; src = [t1;munchExp e2]; jump = None})
      | (Ir.MEM e1, e2) ->
         emit (As.OPER {assem = "sw `s1,(`s0)\n";
                       dst = []; src = [munchExp e1;munchExp e2]; jump = None}) 

      | (Ir.TEMP t, e) ->
         emit (As.MOVE {assem = "move `d0,`s0" ^ "\n";
                       dst = t; src = munchExp e})
      | (e, Ir.CONST c) -> 
        emit(As.OPER {assem = "li `d0,c\n";
                      dst = [munchExp e]; src = [];jump = None})         

      | _ -> incomplete ()   (* Add code here *)

    (* muncExp has the type Ir.exp -> Tm.temp, i.e. returns a register
       that will bear the results of evaluation the expression 
     *)
    and munchExp = function
      | Ir.CONST c ->
         let t = Tm.new_temp()
         in emit (As.OPER {assem ="li `d0," ^ (string_of_int c) ^ "\n";
                          dst = [t]; src = []; jump = None}) ;
            t

      | Ir.NAME lab ->
         let t = Tm.new_temp()
         in emit (As.OPER {assem ="la `d0," ^ (Tm.lab_name lab) ^ "\n";
                          dst = [t]; src = []; jump = None}) ;
            t

      | Ir.TEMP t -> t

      | Ir.BINOP (Ir.PLUS, Ir.CONST 0, e) -> 
        (munchExp e)

      | Ir.BINOP (Ir.PLUS, e, Ir.CONST 0) -> 
        let t = Tm.new_temp() 
        in emit (As.MOVE {assem = "move `d0, `s0\n";
                          dst = t; src = munchExp e;});
        t

      | Ir.BINOP (Ir.PLUS, Ir.CONST c1, Ir.CONST c2) -> 
        let t = Tm.new_temp() 
        in emit (As.OPER {assem = "li `d0," ^ (string_of_int (c1 + c2)) ^ "\n";
                          dst = [t]; src = []; jump = None});
        t

      | Ir.BINOP (Ir.PLUS, e1, Ir.CONST c) -> 
        let t = Tm.new_temp() 
        in emit (As.OPER {assem = "addi `d0,`s0," ^ (string_of_int c) ^ "\n";
                          dst = [t]; src = [munchExp e1]; jump = None});
        t

      | Ir.BINOP (Ir.PLUS, Ir.CONST c, e1) -> 
        let t = Tm.new_temp() in 
        emit (As.OPER {assem = "addi `d0,`s0," ^ (string_of_int c) ^ "\n";
                       dst = [t]; src = [munchExp e1]; jump = None});
        t

      | Ir.BINOP (Ir.PLUS, e1, e2) -> 
        let t = Tm.new_temp() 
        in emit (As.OPER {assem = "add `d0,`s0,`s1" ^ "\n";
                          dst = [t]; src = [munchExp e1; munchExp e2]; jump = None});
        t

      | Ir.BINOP (Ir.MINUS, Ir.CONST c1, Ir.CONST c2) -> 
        let t = Tm.new_temp() 
        in emit (As.OPER {assem = "li `d0," ^ (string_of_int (c1 - c2)) ^ "\n";
                          dst = [t]; src = []; jump = None});
        t

      | Ir.BINOP (Ir.MINUS, e1, e2) -> 
        let t = Tm.new_temp() 
        in emit (As.OPER {assem = "sub `d0,`s0,`s1" ^ "\n";
                          dst = [t]; src = [munchExp e1; munchExp e2]; jump = None});
        t

      | Ir.BINOP (Ir.MUL, Ir.CONST c1, Ir.CONST c2) -> 
        let t = Tm.new_temp() 
        in emit (As.OPER {assem = "li `d0," ^ (string_of_int (c1 * c2)) ^ "\n";
                          dst = [t]; src = []; jump = None});
        t
      | Ir.BINOP (Ir.MUL, e1, e2) -> 
        let t = Tm.new_temp() 
        in emit (As.OPER {assem = "mul `d0,`s0,`s1" ^ "\n";
                          dst = [t]; src = [munchExp e1; munchExp e2]; jump = None});
        t
      | Ir.BINOP(Ir.DIV, ((Ir.CONST i1) as e1), ((Ir.CONST 0) as e2)) ->
        let t = Tm.new_temp() 
        in emit (As.OPER {assem ="div `d0,`s0,`s1" ^ "\n";
                          dst = [t]; src = [munchExp e1; munchExp e2]; jump = None});
          t 
      | Ir.BINOP (Ir.DIV, Ir.CONST c1, Ir.CONST c2) -> 
        let t = Tm.new_temp() 
        in emit (As.OPER {assem = "li `d0," ^ (string_of_int (c1 / c2)) ^ "\n";
                          dst = [t]; src = []; jump = None});
        t

      | Ir.BINOP (Ir.DIV, e1, e2) -> 
        let t = Tm.new_temp() 
        in emit (As.OPER {assem = "div `d0,`s0,`s1" ^ "\n";
                          dst = [t]; src = [munchExp e1; munchExp e2]; jump = None});
        t
      | Ir.CALL (lexp, args) ->
        begin match lexp with 
        | Ir.NAME l ->
          let exps = (List.map munchExp args) in
          let curOffset = (List.length args) in
          let src1 = (munchArgs 0 exps curOffset) in
          emit (As.OPER {assem = "jal " ^ (Tm.lab_name l) ^ "\n";
                          dst = Mr.call_defs; src = src1; jump = None}) ;
          Mr.rv_reg
        end

      | Ir.MEM (Ir.CONST i) ->
         let t = Tm.new_temp()
         in emit (As.OPER {assem ="lw `d0," ^ (string_of_int i) ^ "($zero)\n";
                          dst = [t]; src = []; jump = None});
            t
      | Ir.MEM (Ir.BINOP(Ir.PLUS, e1, Ir.CONST c)) ->
         let t = Tm.new_temp()
         in emit (As.OPER {assem ="lw `d0," ^ (string_of_int c) ^"(`s0)\n";
                          dst = [t]; src = [munchExp e1]; jump = None});
            t 
      | Ir.MEM (Ir.BINOP(Ir.PLUS, Ir.CONST c, e1)) ->
         let t = Tm.new_temp()
         in emit (As.OPER {assem ="lw `d0," ^ (string_of_int c) ^"(`s0)\n";
                          dst = [t]; src = [munchExp e1]; jump = None});
            t
      | Ir.MEM (Ir.BINOP(Ir.MINUS, e1, Ir.CONST c)) ->
         let t = Tm.new_temp()
         in emit (As.OPER {assem ="lw `d0,-" ^ (string_of_int c) ^"(`s0)\n";
                          dst = [t]; src = [munchExp e1]; jump = None});
            t 
      | Ir.MEM (Ir.BINOP(Ir.MINUS, Ir.CONST c, e1)) ->
         let t = Tm.new_temp()
         in emit (As.OPER {assem ="lw `d0,-" ^ (string_of_int c) ^"(`s0)\n";
                          dst = [t]; src = [munchExp e1]; jump = None});
            t                               
      | Ir.MEM (e) ->
         let t = Tm.new_temp()
         in emit (As.OPER {assem ="lw `d0," ^ "(`s0)\n";
                          dst = [t]; src = [munchExp e]; jump = None});
            t

      | _ -> incomplete ()

    and munchArgs i li offset = match i, li with
      | i, [] -> []
      | i, exp :: rest ->
        if i < 4 then begin
          let dst = (Array.get Mr.argregs i) in
            (munchStm (Ir.MOVE(Ir.TEMP dst, Ir.TEMP exp)));
            dst :: (munchArgs (i + 1) rest offset)
          end
        else begin
            (munchStm(Ir.MOVE(Ir.MEM(Ir.BINOP(Ir.MINUS, Ir.TEMP Mr.sp_reg, Ir.CONST ((offset - i)*4))), Ir.TEMP exp)));
               (munchArgs (i + 1) rest offset)
        end


    in munchStmList stml ; 
       (List.rev ( (As.OPER {assem =""; 
                             dst = []; src = Mr.return_sink;
                             jump = None}) ::
                     ( !ilist )
                 )
       )

end