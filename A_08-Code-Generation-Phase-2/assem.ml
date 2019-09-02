(* Code courtesy of Gopalan Nadathur. 

   Translted from Standard ML to OCaml by Eric Van Wyk.
 *)

module type ASSEM = sig
  type temp 
  type label

  type instr = OPER of {assem: string;
	 		dst: temp list;
			src: temp list;
			jump: label list option}
             | LABEL of {assem: string; 
                         lab: label}
             | MOVE of {assem: string;
			dst: temp;
			src: temp}

  val format : (temp -> string) -> instr -> string
end

module Assem (Tm: Temp.TEMP) 
       : (ASSEM with type temp = Tm.temp
                 and type label = Tm.label)
  = struct

  type temp = Tm.temp
  type label = Tm.label

  type instr = OPER of {assem: string;
	 		dst: temp list;
			src: temp list;
			jump: label list option}
             | LABEL of {assem: string; 
                         lab: label}
             | MOVE of {assem: string;
			dst: temp;
			src: temp}

  let implode (cs: char list) : string =
    String.concat "" (List.map  (String.make 1) cs)

  let explode (s: string) : char list =
    let l = String.length s
    in
    let rec f i = 
      if i = l then [] else s.[i] :: f (i+1)
    in f 0

  let format saytemp inst =
    let speak (assem, dst, src, jump) =
      let saylab = Tm.lab_name in
      let rec f instr_chrs = match instr_chrs with
        | '`' :: 's':: i :: rest ->
	  (explode (saytemp (List.nth src (Char.code i - Char.code '0'))))
          @ f rest

	| '`' :: 'd' :: i :: rest ->
	   (explode (saytemp (List.nth dst (Char.code i - Char.code '0'))) 
            @ f rest)

        | '`' :: 'j' :: i :: rest ->
	   (explode (saylab (List.nth jump (Char.code i - Char.code '0'))) 
            @ f rest)
	| '`' :: '`' :: rest -> '`' :: f rest
	| '`' :: _ :: rest -> Errormsg.ErrorMsg.impossible "bad Assem format"
	| c :: rest -> c :: f rest
	| [] -> []
      in implode (f (explode assem))

    in match inst with
       | OPER {assem; dst; src; jump=None} -> 
          speak (assem, dst, src, [])
       | OPER {assem; dst; src; jump=Some j} -> 
          speak (assem, dst, src, j)
       | LABEL {assem} -> assem
       | MOVE {assem; dst; src} -> 
          speak (assem, [dst], [src], [])

end

