module type SYMBOL = sig
  type symbol = string
  val symbol : string -> symbol
  val name: symbol -> string
end

module Symbol : SYMBOL = struct
  type symbol = string

  (* These functions are just the identify for now, in the next iteration
     we will treat symbols and names differently.
   *)
  let symbol s = s
  let name s = s
end
