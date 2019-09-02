(* This file specifies the execution of the Tiger compiler.  It pulls
   together the Scanner, Parser, and Analysis moduldes.

   It is based a similar file written by Gopalan Nadathur in Standard
   ML.  Some modifications were needed to accommodate the some
   limitation in OCaml-Yacc.  Additionally it supports running the
   compiler on a single file when it is the only command-line
   argument.
 *)

module Driver = struct

  module Er = Analysis.Er
  module Ab = Analysis.Ab
  module Se = Analysis.Se
  module En = Analysis.En
  module Pa = Analysis.Pa
  module Pt = Analysis.Pt

  type result = Ab.exp * En.ty * En.err list
  type out_chans = {syn_chan: out_channel; ast_chan: out_channel;
                    typ_chan: out_channel; err_chan: out_channel;
                    erv_chan: out_channel}

  let process_file (filename: string) (out_chans: out_chans) 
        (task: string -> out_chans -> result -> unit) : unit =

    let in_channel = open_in filename in
    let buffer = Lexing.from_channel in_channel in

    Er.reset ();
    Er.filename := filename;

    try
      let ast : Ab.exp = Parser.program Scanner.scan buffer in
      let (ty,errs) = Se.transProg ast in
      task filename out_chans (ast, ty, errs)
    with 
    | Scanner.Lexical_error -> 
       Er.errorf out_chans.syn_chan (Lexing.lexeme_start buffer ) 
                      "A lexical error occurred." 
    | Parsing.Parse_error -> 
       Er.errorf out_chans.syn_chan (Lexing.lexeme_start buffer ) 
                      "A syntax error occurred." 
    

  (* Functions to display output *)
  let print_absyn (filename: string) (out_chans: out_chans)
                  (res: result) : unit =
    match res with ast, typ, errs ->
      Printf.printf "\nWriting absyn for %s\n" filename;
      Pa.print out_chans.ast_chan ast


  let print_all (verbose: bool) (filename: string) (out_chans: out_chans)
                (res: result) : unit =
    match res with ast, typ, errs ->
      (if verbose 
       then Printf.printf "\nWriting absyn  for %s\n\n" filename
       else () );
      Pa.print out_chans.ast_chan ast;

      (if verbose 
       then Printf.printf "\nWriting type   for %s\n\n" filename
       else () );
      Pt.print out_chans.typ_chan typ;

      (if verbose 
       then Printf.printf "\nWriting errs   for %s\n\n" filename
       else () );
      Pt.print_errs out_chans.err_chan errs false;

      (if verbose 
       then Printf.printf "\nWriting errs-v for %s\n\n" filename
       else () );
      Pt.print_errs out_chans.erv_chan errs true


  let run_tiger filename out_dir =
    Printf.printf "\n\nProcessing file    %s\n" filename;
    let out_chans = { syn_chan = stdout; ast_chan = stdout;
                      typ_chan = stdout; err_chan = stdout;
                      erv_chan = stdout}
    in
    process_file filename out_chans (print_all true)

  let test_tiger filename out_dir =
    Printf.printf "Processing file   %s\n" filename;
    let syn_filename =
      (Filename.concat out_dir (Filename.basename filename)) ^ ".syn" in
    let ast_filename =
      (Filename.concat out_dir (Filename.basename filename)) ^ ".absyn" in
    let typ_filename =
      (Filename.concat out_dir (Filename.basename filename)) ^ ".type" in
    let err_filename =
      (Filename.concat out_dir (Filename.basename filename)) ^ ".errs" in
    let erv_filename =
      (Filename.concat out_dir (Filename.basename filename)) ^ ".errs-v" in
    let out_chans = {
        syn_chan = open_out syn_filename;
        ast_chan = open_out ast_filename;
        typ_chan = open_out typ_filename;
        err_chan = open_out err_filename;
        erv_chan = open_out erv_filename } in
    process_file filename out_chans (print_all false);
    close_out out_chans.syn_chan;
    close_out out_chans.ast_chan;
    close_out out_chans.typ_chan;
    close_out out_chans.err_chan;
    close_out out_chans.erv_chan

  let test_all t d o =
    Printf.printf "Testing .tig files in directory\n    %s.\n" d;
    let all_files = Array.to_list (Sys.readdir d) in
    let is_tiger f = Filename.check_suffix f ".tig" in
    let tig_files = List.filter is_tiger all_files
    in List.fold_left 
         (fun _ f -> t (Filename.concat d f) o)
         ()
         (List.sort String.compare tig_files)
 
  (* The function above takes a directory name under Testing/TestCases
     for example "Appel" or "Initial" and runs the parser on all the
     .tig files in that directory.

     It should put the results in a directory of this same name
     (e.g. "Appel" or "Initial") in the Testing/CorrectOuptput/Parser/
     directory.
   *)


  (* The executable takes 1 or 2 command line arguments, in addition
     to the name of the executabe (thus 2 and 3 below) *)
  let () =
    match Array.length Sys.argv with
    | 3 ->  (* run tests for all files in a directory *)
       let input_dirname = Sys.argv.(1) in
       let output_dirname = Sys.argv.(2) in
       test_all test_tiger input_dirname output_dirname
    | 2 ->  (* run the compiler on a file named on the command line *)
       let filename = Sys.argv.(1) in
       run_tiger filename "NOTUSED"
    | _ ->
       Printf.printf 
         "Incorrect usage: number of arguments should be 2 or 3, " ;
      Printf.printf "it was %d.\n\n" (Array.length Sys.argv)


end
