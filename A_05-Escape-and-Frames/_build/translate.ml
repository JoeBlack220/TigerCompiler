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
end


(* Courtesy of Gopalan Nadathur.
   Minor changes by Eric Van Wyk to avoid nonexhaustive match warnings.
 *)

module Translate (Fr: Frame.FRAME)
                 (Tm: Temp.TEMP with type label = Fr.label)
       : (TRANSLATE with type label = Tm.label) = struct

  type unique = unit ref

  type level = Bottom 
             | Other of Fr.frame * level * unique 

  type access = level * Fr.access

  type label = Tm.label

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

end
   

