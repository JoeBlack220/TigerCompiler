(* Standard ML code courtesy of Gopalan Nadathur.  

   Translation to OCaml by Eric Van Wyk. 
 *)

module MipsFrame (Tm: Temp.TEMP)
       : (Frame.FRAME  with type label = Tm.label)
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

  (* multiplier for words *)
  let ws = 4
  let num_args_regs = 4

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

end


         
