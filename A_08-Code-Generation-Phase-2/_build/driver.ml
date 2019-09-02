(* This file specifies the execution of the Tiger compiler.  It pulls
   together the Scanner, Parser, and Analysis moduldes.

   It is based a similar file written by Gopalan Nadathur in Standard
   ML.  Some modifications were needed to accommodate the some
   limitation in OCaml-Yacc.  Additionally it supports running the
   compiler on a single file when it is the only command-line
   argument.
 *)

module type DRIVER = sig
  val run_tiger : string -> string -> string -> unit
  val test_all : (string -> string -> string -> unit) ->
                 string -> string -> string -> unit
end

module Driver : DRIVER = struct

  module Er = Analysis.Er
  module Ab = Analysis.Ab
  module Se = Analysis.Se
  module En = Analysis.En
  module Pa = Analysis.Pa
  module Pt = Analysis.Pt
  module Es = Analysis.Es
  module Fr = Analysis.Fr
  module Tr = Analysis.Tr
  module Pi = Analysis.Pi
  module As = Analysis.As
  module Mr = Analysis.Mr
  module Mc = Analysis.Mc
  module Ra = Analysis.Ra

  type result = Ab.exp * Se.transty * En.err list * Fr.frag list

  type out_chans = 
    { syn_chan: out_channel; ast_chan: out_channel; typ_chan: out_channel; 
      err_chan: out_channel; erv_chan: out_channel;
      frg_chan: out_channel; frn_chan: out_channel;
      asm_chan: out_channel
    }

  let process_file (filename: string) (out_chans: out_chans) 
        (task: string -> out_chans -> result -> string -> unit) 
        (phase: string) : unit =

    let in_channel = open_in filename in
    let buffer = Lexing.from_channel in_channel in

    Er.reset ();
    Er.filename := filename;
    Tr.init_frags ();

    try
      let ast : Ab.exp = Parser.program Scanner.scan buffer in
      let () = Es.find_escapes ast in
      let (ty,errs) = Se.transProg ast in
      let frags = Tr.get_results () in
      task filename out_chans (ast, ty, errs, frags) phase
    with 
    | Scanner.Lexical_error -> 
       Er.errorf out_chans.syn_chan (Lexing.lexeme_start buffer ) 
                      "A lexical error occurred." 
    | Parsing.Parse_error -> 
       Er.errorf out_chans.syn_chan (Lexing.lexeme_start buffer ) 
                      "A syntax error occurred." 
    

  (* Functions to display output *)
  let print_absyn (filename: string) (out_chans: out_chans)
                  (res: result) (phase: string) : unit =
    match res with ast, (tr, typ), errs, frags ->
      Printf.printf "\nWriting absyn for %s\n" filename;
      Pa.print out_chans.ast_chan ast


  (* translate f s: returns the string generated from s by mapping 
     each character in s by f *)
  let translate f s = 
    let explode (s: string) : char list =
      let l = String.length s
      in
      let rec f i = 
        if i = l then [] else s.[i] :: f (i+1)
      in f 0
    in
    String.concat "" (List.map f (explode s))

  let emit_mips_phase_1 out_chan frags = 
    let say msg = Printf.fprintf out_chan "%s" msg in
    let sayln msg = (say msg; say "\n") in
    let emit frag =
      if Fr.string_frag_b frag
      then
        (say "LABEL ";
         sayln (Fr.string_frag_lab frag);
         sayln (Fr.string_frag_str frag ^ "\"");
         sayln ""
        )
      else
        (let instrs = Mc.codegen (Fr.proc_frag_body frag) 
         in
         let format0 = As.format (Mr.tempmap (fun t -> t))
         in (say "LABEL ";
             sayln (Fr.proc_frag_lab frag);
             List.iter (fun i -> say (format0 i)) instrs;
             sayln ""
            )
        )
    in List.iter emit frags

  let emit_runtime out_chan =
    let in_chan = open_in "runtime.assem" in
    try
      while true; do
        Printf.fprintf out_chan "%s\n" (input_line in_chan)
      done
    with End_of_file ->
      close_in in_chan

  let emit_mips_phase_2 out_chan frags = 
    let say msg = Printf.fprintf out_chan "%s" msg in
    let sayln msg = (say msg; say "\n") in
    let emit frag = 
      if Fr.string_frag_b frag
      then
        (sayln ".data\n.align 2";
         sayln (Fr.string_frag_lab frag ^ ":");
         say ".asciiz \"";
         let trans_f x = if x = '\"' then "\\\"" else String.make 1 x in
         let translated = translate trans_f (Fr.string_frag_str frag) in
         sayln (translated ^ "\"\n")
        )
      else 
        (let instrs = Mc.codegen (Fr.proc_frag_body frag) 
         in
         let (instrs, temp_map) = 
           Ra.regalloc instrs (Mr.all_regs) (Fr.procFragFrame frag)
         in
         let format0 = As.format (Mr.tempmap temp_map)
         in (sayln ".text";
             sayln (Fr.proc_frag_lab frag ^ ":");
             List.iter (fun i -> say (format0 i)) instrs;
             sayln ""
            )
        )
    in List.iter emit frags



  let print_all (verbose: bool) (filename: string) (out_chans: out_chans)
                (res: result) (phase: string) : unit =
    match res with ast, (tr, typ), errs, frags ->
      (if verbose 
       then Printf.printf "\nWriting absyn   for %s\n\n" filename
       else () );
      Pa.print out_chans.ast_chan ast;

      (if verbose 
       then Printf.printf "\nWriting type    for %s\n\n" filename
       else () );
      Pt.print out_chans.typ_chan typ;

      (if verbose 
       then Printf.printf "\nWriting errs    for %s\n\n" filename
       else () );
      Pt.print_errs out_chans.err_chan errs false;

      (if verbose 
       then Printf.printf "\nWriting errs-v  for %s\n\n" filename
       else () );
      Pt.print_errs out_chans.erv_chan errs true;

      (if verbose 
       then Printf.printf "\nWriting frags   for %s\n\n" filename
       else () );
      (match errs with
       | [] -> Pi.print_prog out_chans.frg_chan frags "t"
       | _ -> ()
      );

      (if verbose 
       then Printf.printf "\nWriting frags-n for %s\n\n" filename
       else () );
      (match errs with
       | [] -> Pi.print_prog out_chans.frn_chan (Pi.normalize frags) "tr"
       | _ -> ()
      );

      (if verbose 
       then Printf.printf "\nWriting assem for %s\n\n" filename
       else () );
      (match errs, phase with
       | [], "1" -> emit_mips_phase_1 out_chans.asm_chan frags
       | [], "2" -> emit_runtime out_chans.asm_chan; emit_mips_phase_2 out_chans.asm_chan frags
       | _ -> ()
      )




  let run_tiger (filename: string) (out_dir: string) (phase: string) : unit =
    Printf.printf "\n\nProcessing file    %s\n" filename;
    let out_chans = { syn_chan = stdout; ast_chan = stdout;
                      typ_chan = stdout; err_chan = stdout;
                      erv_chan = stdout; frg_chan = stdout;
                      frn_chan = stdout; asm_chan = stdout}
    in
    process_file filename out_chans (print_all true) phase

  let test_tiger (filename: string) (out_dir: string) (phase: string) 
      : unit =
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
    let frg_filename =
      (Filename.concat out_dir (Filename.basename filename)) ^ ".frags" in
    let frn_filename =
      (Filename.concat out_dir (Filename.basename filename)) ^ ".frags-n" in
    let asm_filename =
      (Filename.concat out_dir (Filename.basename filename)) ^ ".assem" in
    let out_chans = {
        syn_chan = open_out syn_filename;
        ast_chan = open_out ast_filename;
        typ_chan = open_out typ_filename;
        err_chan = open_out err_filename;
        erv_chan = open_out erv_filename;
        frg_chan = open_out frg_filename;
        frn_chan = open_out frn_filename;
        asm_chan = open_out asm_filename 
      } in
    process_file filename out_chans (print_all false) phase;
    close_out out_chans.syn_chan;
    close_out out_chans.ast_chan;
    close_out out_chans.typ_chan;
    close_out out_chans.err_chan;
    close_out out_chans.erv_chan;
    close_out out_chans.frg_chan;
    close_out out_chans.frn_chan;
    close_out out_chans.asm_chan

  let test_all (t: string -> string -> string -> unit) 
               (d: string) (o: string) (phase: string) : unit =
    Printf.printf "Testing .tig files in directory\n    %s.\n" d;
    let all_files = Array.to_list (Sys.readdir d) in
    let is_tiger f = Filename.check_suffix f ".tig" in
    let tig_files = List.filter is_tiger all_files
    in List.fold_left 
         (fun _ f -> t (Filename.concat d f) o phase)
         ()
         (List.sort String.compare tig_files)
 
  (* The function above takes a directory name under Testing/TestCases
     for example "Appel" or "Initial" and runs the parser on all the
     .tig files in that directory.

     It should put the results in a directory of this same name
     (e.g. "Appel" or "Initial") in the Testing/CorrectOuptput/Parser/
     directory.
   *)


  (* The executable takes 2 or 3 command line arguments, in addition
     to the name of the executabe (thus 3 and 4 below) *)
  let () =
    match Array.length Sys.argv with
    | 4 ->  (* run tests for all files in a directory *)
       let input_dirname = Sys.argv.(1) in
       let output_dirname = Sys.argv.(2) in
       let phase = Sys.argv.(3) in
       test_all test_tiger input_dirname output_dirname phase
    | 3 ->  (* run the compiler on a file named on the command line *)
       let filename = Sys.argv.(1) in
       let phase = Sys.argv.(2) in
       run_tiger filename "NOTUSED" phase
    | _ ->
       Printf.printf 
         "Incorrect usage: number of arguments should be 3 or 4, " ;
      Printf.printf "it was %d.\n\n" (Array.length Sys.argv)


end
