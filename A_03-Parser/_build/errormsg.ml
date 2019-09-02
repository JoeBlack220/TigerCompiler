module type ERRORMSG = sig
  val any_errors : bool ref
  val filename : string ref
  val linenum : int ref
  val linepos : int list ref
  val error : int -> string -> unit
  val errorf : out_channel -> int -> string -> unit
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

  let errorf channel pos (msg : string) =
    let rec look (ps, n) =
      match ps with
        p::rest ->
          if p < pos
          then Printf.fprintf channel ":%d.%d" n (pos-p)
          else look (rest, n-1)
      | [] -> Printf.fprintf channel "0.0"
    in
      any_errors := true;
      Printf.fprintf channel "%s" (Filename.basename !filename);
      look (!linepos, !linenum);
      Printf.fprintf channel ":%s\n" msg

  let error = errorf stdout

  let impossible msg =
    Printf.printf "Error: Compiler bug: %s\n" msg;
    raise Error
end

