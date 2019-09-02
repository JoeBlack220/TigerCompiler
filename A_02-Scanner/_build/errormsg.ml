(* OCaml version of SML program by Gopalan Nadathur. *)

module type ERRORMSG = sig
  val any_errors : bool ref
  val filename : string ref
  val linenum : int ref
  val linepos : int list ref
  val error : int -> string -> unit
  exception Error
  val impossible : string -> 'a (* raises Error *)
  val reset : unit -> unit
end

module ErrorMsg : ERRORMSG = struct
  let any_errors = ref false
  let filename = ref ""
  let linenum = ref 1
  let linepos = ref [-1]
  
  let reset _ =
    any_errors := false;
    filename := "";
    linenum := 1;
    linepos := [-1]

  exception Error

  let error pos (msg : string) =
    let rec look (ps, n) =
      match ps with
        p::rest ->
          if p < pos
          then Printf.printf ":%d.%d" n (pos-p)
          else look (rest, n-1)
      | [] -> Printf.printf "0.0"
    in
      any_errors := true;
      Printf.printf "%s" !filename;
      look (!linepos, !linenum);
      Printf.printf ":%s\n" msg

  let impossible msg =
    Printf.printf "Error: Compiler bug: %s\n" msg;
    raise Error
end

