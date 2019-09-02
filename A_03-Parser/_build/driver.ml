
open Errormsg
open Printabsyn

module Driver = struct

  let parse_file filename out_channel =
    let in_channel = open_in filename in
    let buffer = Lexing.from_channel in_channel in

    ErrorMsg.reset ();
    ErrorMsg.filename := filename;

    try
      let result = Parser.program Scanner.scan buffer in
      Printabsyn.print out_channel result
    with 
    | Scanner.Lexical_error -> 
       ErrorMsg.errorf out_channel (Lexing.lexeme_start buffer ) 
                      "A lexical error occurred." 
    | Parsing.Parse_error -> 
       (* This might be useful if you want to see the value of linepos.
       let t1 = List.map string_of_int (! ErrorMsg.linepos) in
       let s1 = String.concat ", " t1 in
       Printf.printf "linepos is %s\n" s1;
       *)
       ErrorMsg.errorf out_channel (Lexing.lexeme_start buffer ) 
                      "A syntax error occurred." 


  let run_parser filename out =
    Printf.printf "Parsing file %s.\n" filename;
    let output_filename =
      (Filename.concat out (Filename.basename filename)) ^ ".out" in
    let out_channel = open_out output_filename in

    Printf.printf "Writing to %s.\n" output_filename;
    parse_file filename out_channel


  let test_parser d o =
    Printf.printf "Testing .tig files in directory %s.\n" d;
    let all_files = Array.to_list (Sys.readdir d) in
    let is_tiger f = Filename.check_suffix f ".tig" in
    let tig_files = List.filter is_tiger all_files
    in List.fold_left 
         (fun _ f -> run_parser (Filename.concat d f) o)
         ()
         (List.sort String.compare tig_files)
 
  (* The function above takes a directory name under Testing/TestCases
     for example "Appel" or "Initial" and runs the parser on all the
     .tig files in that directory.

     It should put the results in a directory of this same name
     (e.g. "Appel" or "Initial") in the Testing/CorrectOuptput/Parser/
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
      test_parser dirname output_dirname

end
