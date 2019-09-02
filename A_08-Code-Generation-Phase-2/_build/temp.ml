(* Code from Andrew Appel.
   Translated from Standard ML to OCaml by Eric Van Wyk.
 *)

module type TEMP = sig
  type temp
  val new_temp : unit -> temp
  val make_string: temp -> string

  type label
  val new_label : unit -> label
  val named_label : string -> label
(*
  module Tbl : Table.TABLE with type key = temp 
 *)
  type 'a table
  val empty : 'a table
  val enter : 'a table -> temp -> 'a -> 'a table
  val look  : 'a table -> temp -> 'a option

  (* Some hacks for printing *)
  val lab_name: label -> string
  val temp_name: temp -> string

  (* A hack for normalizing *)
  val named_temp: int -> temp 

end


module Temp (Sy: Symbol.SYMBOL) : TEMP with type label = Sy.symbol = struct
  type temp = int
  let temps = ref 100
                  
  let new_temp () = let t = ! temps in temps := t + 1; t

  let make_string t = "t" ^ string_of_int t

  type label = Sy.symbol

  let labels = ref 0
  let new_label () = 
    let str = Printf.sprintf "L%d" (!labels) in
    let () = labels := !labels + 1 in
    Sy.symbol str

  let named_label = Sy.symbol

(*
  module Ky : Table.KEY = struct
    type key = temp
    let compare k1 k2 = Pervasives.compare k1 k2
  end

  module Tbl = Table.Table(Ky) 
 *)

  type 'a table = (temp * 'a) list
  let empty = []
  let enter t k v = (k, v) :: t
  let look t s' = 
    match List.find_opt (fun (s, v) -> s = s') t with
    | None -> None
    | Some (s, v) -> Some v


  let lab_name (lab: label) = Sy.name lab
  let temp_name (t: temp) = string_of_int t
  let named_temp (s: int) : temp = s

end
