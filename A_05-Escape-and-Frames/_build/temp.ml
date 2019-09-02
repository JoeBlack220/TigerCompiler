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
end


module Temp (S: Symbol.SYMBOL) : TEMP = struct
  type temp = int
  let temps = ref 100
                  
  let new_temp () = let t = ! temps in temps := t + 1; t

  let make_string t = "t" ^ string_of_int t

  type label = S.symbol

  let labels = ref 0
  let new_label () = 
    let str = Printf.sprintf "L%d" (!labels) in
    let () = labels := !labels + 1 in
    S.symbol str

  let named_label = S.symbol

end

