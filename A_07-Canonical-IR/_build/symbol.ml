(* The orinal Standard ML version of this file was written by
   Gopalan Nadathur.

   This translation to OCaml is by Eric Van Wyk.
 *)

module type SYMBOL = sig
  type symbol
  val symbol : string -> symbol
  val name: symbol -> string

  type 'a table
  val empty : 'a table
  val enter : 'a table -> symbol -> 'a -> 'a table
  val look  : 'a table -> symbol -> 'a option
end

module Symbol : SYMBOL = struct
  type symbol = string * int

  module H = Hashtbl

  let next_sym = ref 0 
  let size = 128
  let hash_table : (string, int) H.t = H.create size

  let symbol name = 
    match H.find_opt hash_table name with
    | Some i -> (name, i)
    | None -> let i = !next_sym in
              next_sym := i + 1;
              H.add hash_table name i;
              (name, i)

  let name (s,n) = s

  (* This is a very inefficient implementation of a functional symbol
     table.  Can you do better?
   *)
  type 'a table = (symbol * 'a) list
  let empty = []
  let enter t s v = (s, v) :: t
  let look t s' = 
    match List.find_opt (fun (s, v) -> s = s') t with
    | None -> None
    | Some (s, v) -> Some v

end
