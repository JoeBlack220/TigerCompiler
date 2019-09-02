(* OCaml version of SML program by Gopalan Nadathur. *)

open Tokens_mod
open Errormsg
open Lex

module Driver = struct

  let scan_file f =
    let in_channel = open_in f in
    let buffer = Lexing.from_channel in_channel in
    let rec scan_all (buf1: Lexing.lexbuf) =
      match Scanner.scan buf1 with
      | (Tokens.EOF p, _) -> [Tokens.EOF p]
      | (t, buf2) -> t :: scan_all buf2
    in
    ErrorMsg.reset ();
    ErrorMsg.filename := f;
    scan_all buffer

  let run_scanner f o =
    Printf.printf "Scanning %s.\n" f;
    let tokens = scan_file f in
    let s = String.concat "\n" (List.map Tokens.string_of_token tokens) in
    let output_filename = (Filename.concat o (Filename.basename f)) ^ ".out" in
    let out_channel = open_out output_filename in
    Printf.printf "Writing to %s.\n" output_filename;
    Printf.fprintf out_channel "%s" s;
    Printf.fprintf out_channel "\n"
    (* print_endline s *)
    (* This should run the scanner on the named file, printint results
       to the screen. *)


  let test_scanner d o =
    Printf.printf "Testing .tig files in directory %s.\n" d;
    let all_files = Array.to_list (Sys.readdir d)
    in
    let tig_files = List.filter (fun f -> Filename.check_suffix f ".tig") all_files
    in
    List.fold_left (fun _ f -> run_scanner (Filename.concat d f) o) ()
                   (List.sort String.compare tig_files)
    (* This takes a directory name under Testing/TestCases for example
       "Appel" or "Initial" and runs the scanner on all the .tig files
       in that directory.

       It should put the results in a directory of this same name
      (e.g. "Appel" or "Initial") in the Testing/CorrectOuptput/Scanner/
      directory.
    *)


  let () =
    if Array.length Sys.argv <> 3
    then (
      Printf.printf "Incorrect usage: number of arguments should be 2, " ;
      Printf.printf "it was %d.\n\n" (Array.length Sys.argv)
    )
    else
      let dirname = Sys.argv.(1) in
      let output_dirname = Sys.argv.(2) in
      test_scanner dirname output_dirname

end
