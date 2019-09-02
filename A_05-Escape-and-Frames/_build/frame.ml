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
end
