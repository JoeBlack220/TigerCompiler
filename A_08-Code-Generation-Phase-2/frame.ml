(* Standard ML code from Appel.

   Translation to OCaml by Eric Van Wyk.
 *)

module type FRAME = sig
  type frame
  type label
  type access
  type frame_init = {name: label; formals: bool list}
  val new_frame: frame_init -> frame
  val name: frame -> label
  val formals: frame -> access list
  val alloc_local: frame -> bool -> access

  (* New for A_06 *)

  (* these are needed because the frame has to determine how to wrap
     intermediate language code for a procedure to realize view shifts;
     right now this is done through procBody
   *)
  type exp 
  type stm
  type temp

  (* word size; may be needed in estimating space requests for arrays
     and records in translation 
   *)
  val ws: int  

  (* this one takes the expression and the frame (that contains, amongst
     other things, the label to be inserted at the end of the code for the
     body) corresponding to the procedure body and returns a statement 
     corresponding to the procedure with the appropriate view shift wrapper 
     inserted.
   *)
  val proc_body : frame -> exp -> stm

  (* used to get to the label for the beginning of procedure epilogue part *)
  val proc_epilogue : frame -> label

  (* treatment of external calls; this one belongs here because 
     how to interface with external code is a machine dependent issue *)
  val external_call: string -> (exp list) -> exp


  (* Declarations for treating fragments, don't really believe this
     should be in the frame code but will stick to Appel's idea for now *)
  type frag

  val string_frag: label -> string -> frag
  val string_frag_b: frag -> bool
  val string_frag_lab: frag -> string
  val string_frag_lab': frag -> label 
  val proc_frag: frame -> stm list -> frag  (* Changed in A-07 *)
  val string_frag_str: frag -> string
  val proc_frag_lab: frag -> string
  val proc_frag_lab': frag -> label
  val proc_frag_body: frag -> stm list      (* Changed in A-07 *)
  (* New for A-08 *)
  type instr
  val procPrologue : frame -> label
  val procFragFrame : frag -> frame

  val entryExit: frame -> instr list -> instr list
  val spillTemps: frame -> temp list -> (temp * access) list

  val loadSpill: temp * access -> instr
  val storeSpill: temp * access -> instr
  val gen_Var_Tr: frame list -> access -> exp
  val gen_Slink_Tr: frame list -> exp
  val getExp: frame list -> exp
  val gen_mulLink: frame -> exp
  val gen_Var_Tr2: frame list -> exp
end
