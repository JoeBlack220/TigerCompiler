(* Code courtesty of Gopalan Nadathur.
   Translated to OCaml by Eric Van Wyk. 
 *)

module type MIPSREGS = sig
   type temp

   (* number of argument register *) 
   val numargsregs : int

   val tempmap : (temp -> temp) -> temp -> string

   (* MIPS registers with specific names *)
   val zero : temp    (* always zero *)
   val rv_reg : temp   (* return value *)
   val sl_reg : temp   (* static link if used *)
   val sp_reg : temp   (* stack pointer *)
   val fp_reg : temp   (* frame pointer *)
   val ra_reg : temp   (* return address register *)
   
   val caller_save_regs : temp array (* caller save regs in indexible form *)
   val callee_save_regs : temp array (* callee save regs in indexible form *)
   val argregs : temp array        (* argument regs in indexible form *)

   (* Registers potentially trashed by a procedure call *)
   val call_defs : temp list

   (* All the MIPS registers, needed in coloring based reg assignment *)
   val all_regs : temp list

   (* Registers that need to be recorded as live at end of procedure body *)
   val return_sink : temp list
end



module MipsRegs (Tm: Temp.TEMP) 
       : MIPSREGS with type temp = Tm.temp = struct

    type temp = Tm.temp

    let sub (a, i) = Array.get a i

    (* number of argument registers *)
    let numargsregs = 4

    (* names for argument registers; use Array.sub(argregs,i) for ith reg *)
    let argregs = Array.of_list [Tm.new_temp(); Tm.new_temp();
                                 Tm.new_temp(); Tm.new_temp()]

    (* caller save registers *)
    let caller_save_regs = Array.of_list [Tm.new_temp(); Tm.new_temp(); 
                                        Tm.new_temp(); Tm.new_temp(); 
                                        Tm.new_temp(); Tm.new_temp(); 
                                        Tm.new_temp(); Tm.new_temp(); 
                                        Tm.new_temp(); Tm.new_temp()]

    (* callee save registers *)
    let callee_save_regs = Array.of_list [Tm.new_temp(); Tm.new_temp(); 
                                        Tm.new_temp(); Tm.new_temp(); 
                                        Tm.new_temp(); Tm.new_temp(); 
                                        Tm.new_temp(); Tm.new_temp()]


    (* names for special MIPS registers *)
    let zero   = Tm.new_temp()        (* always 0 *)
    let rv_reg = Tm.new_temp()        (* return value register *)
    let sl_reg = Tm.new_temp()        (* static link, not used *)
    let fp_reg = Tm.new_temp()        (* frame pointer *)
    let sp_reg = Tm.new_temp()        (* stack pointer *)
    let ra_reg = Tm.new_temp()        (* return address *)

    (* Registers potentially trashed by a procedure call *)
    let call_defs = (sub(caller_save_regs,0)) :: (sub(caller_save_regs,1)) ::
                    (sub(caller_save_regs,2)) :: (sub(caller_save_regs,3)) ::
                    (sub(caller_save_regs,4)) :: (sub(caller_save_regs,5)) ::
                    (sub(caller_save_regs,6)) :: (sub(caller_save_regs,7)) ::
                    (sub(caller_save_regs,8)) :: (sub(caller_save_regs,9)) :: 
                    (sub(argregs,0)) :: (sub(argregs,1)) :: 
                    (sub(argregs,2)) :: (sub(argregs,3)) ::
                   ra_reg :: rv_reg :: sl_reg :: []
 
    (* registers live at the end of the procedure body *)
    let return_sink = rv_reg :: sp_reg :: fp_reg :: []

    (* all the registers *)
    let all_regs = (sub(callee_save_regs,0)) :: (sub(callee_save_regs,1)) ::
                   (sub(callee_save_regs,2)) :: (sub(callee_save_regs,3)) ::
                   (sub(callee_save_regs,4)) :: (sub(callee_save_regs,5)) ::
                   (sub(callee_save_regs,6)) :: (sub(callee_save_regs,7)) ::
                   (sub(caller_save_regs,0)) :: (sub(caller_save_regs,1)) ::
                   (sub(caller_save_regs,2)) :: (sub(caller_save_regs,3)) ::
                   (sub(caller_save_regs,4)) :: (sub(caller_save_regs,5)) ::
                   (sub(caller_save_regs,6)) :: (sub(caller_save_regs,7)) ::
                   (sub(caller_save_regs,8)) :: (sub(caller_save_regs,9)) :: 
                   (sub(argregs,0)) :: (sub(argregs,1)) :: 
                   (sub(argregs,2)) :: (sub(argregs,3)) ::
                   ra_reg :: sl_reg :: return_sink
 


    (* MIPS names for the registers; to be coordinated with all_regs *)
    let reg_names = "$s0" :: "$s1" :: "$s2" :: "$s3" :: 
                    "$s4" :: "$s5" :: "$s6" :: "$s7" :: 
                    "$t0" :: "$t1" :: "$t2" :: "$t3" :: "$t4" :: 
                    "$t5" :: "$t6" :: "$t7" :: "$t8" :: "$t9" :: 
                    "$a0" :: "$a1" :: "$a2" :: "$a3" :: 
                    "$ra" :: "$v1" :: "$v0" :: "$sp" :: "$fp" :: []


    let rec zip l1 l2 = match l1, l2 with
      | [], [] -> []
      | (h1::t1), (h2::t2) -> (h1,h2) :: (zip t1 t2)
      | _, _ -> failwith "zip in mipsregs, unequal length lists"

    (* The next two definitions are for providing a map from 
       temps to names.

utop # List.fold_right ;;
- : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun>
 *)
(*
    module Ky : Table.KEY = struct
      type key = Tm.temp
      let compare = Pervasives.compare
    end

    module Tbl : Table.TABLE with type key = Tm.temp = Table.Table (Ky) 

    let regmap : string Tbl.table = Tbl.empty

    let regmap : string Tbl.table = 
      let ff (tmp, nm) tbl = Tbl.enter tbl tmp nm
      in
      let reg_table = List.fold_right ff (zip all_regs reg_names) Tbl.empty 
      in
      Tbl.enter reg_table zero "$0"

 *)
    let regmap : string Tm.table = Tm.empty

    let regmap : string Tm.table = 
      let ff (tmp, nm) tbl = Tm.enter tbl tmp nm
      in
      let reg_table = List.fold_right ff (zip all_regs reg_names) Tm.empty 
      in
      Tm.enter reg_table zero "$0"

    let tempmap (pm: temp -> temp) (t: temp) : string = 
        let what = Tm.look regmap (pm t)
        in 
        let tempname o t = match o, t with
          | None, t -> Tm.make_string t
          | Some s, _ -> s
        in tempname what t




end                  
