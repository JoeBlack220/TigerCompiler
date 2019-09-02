(* This file is an OCaml translation of code from a Standard ML file
   from Gopalan Nadathur for printing Tiger type and errors.  

   This OCaml translation is by Eric Van Wyk.
*)

module ErrorMsg = Errormsg.ErrorMsg

module type PRINTTYPE = sig
  type ty
  val print : out_channel -> ty -> unit
  type err
  val print_errs : out_channel -> err list -> bool -> unit
end

module Printtype (S: Symbol.SYMBOL) 
                 (E : Env.ENV with type symbol = S.symbol) 
               : (PRINTTYPE with type ty = E.ty and type err = E.err) = struct

  type ty = E.ty
  type err = E.err

  let error = ErrorMsg.error
  let convertPos = ErrorMsg.convertPos

  let printIndent (out_chan, ty, i) =
    let say msg = Printf.fprintf out_chan "%s" msg in
    let sayln msg = (say msg; say "\n") in

    let rec indent = function
      | 0 -> ()
      | i -> (say " "; indent(i-1))
    in

    let rec dolist n xs i k ls = 
      match xs with
      | [] -> ()
      | (sy,ty) :: l ->
         (sayln ""; indent n; 
          let label = S.name sy in
          let len = String.length label in 
          let i' = i ^ "." ^ string_of_int k in
          say (label ^ ": "); 
          print' ty (n+len+2) i' ls;
          dolist n l i (k+1) ls
         )

      and print' ty d i ls = match ty with
        | E.RECORD (l, rd) ->
           (match List.find_opt (fun (x,_) -> x = rd) ls with
            | None -> say ("record" ^ i ^ "{"); 
                      dolist (d+4) l i 1 ((rd,i)::ls); 
                      say "}"
            | Some (_, i) -> say ("record" ^ i)
           ) 
        | E.NIL -> say "NIL"
        | E.INT -> say "INT"
        | E.STRING -> say "STRING"
        | E.ARRAY (ty, _) -> say "array[INT] of "; print' ty (d+14) i ls
        | E.NAME (sy, {contents = Some ty}) -> print' ty d i ls
        | E.NAME (sy, {contents = None}) -> say "PRINTING NONE"
        | E.UNIT -> say "UNIT"
        | E.ERROR  -> say "ERROR"
    in 
    print' ty i "1" []; 
    say "\n"

  let print out ty = printIndent (out, ty, 0)


  let print_errs out_chan errs verbose = 
    let say msg = Printf.fprintf out_chan "%s" msg in
    let sayln msg = (say msg; say "\n") in

    let errorAsInt = function 
      | (_, E.UndeclaredVar _, _) -> 0
      | (_, E.UndeclaredType _, _) -> 1
      | (_, E.UnexpectedType _, _) -> 2
      | (_, E.FuncNameAsVarName _, _) -> 3
      | (_, E.VarNameAsFuncName _, _) -> 4
      | (_, E.TooFewArgs _, _) -> 5
      | (_, E.TooManyArgs _, _) -> 6
      | (_, E.IncorrectType _, _) -> 7
      | (_, E.Duplicates _, _) -> 8
      | (_, E.BreakNotAllowed, _) -> 9
      | (_, E.Circularity, _) -> 10
      | (_, E.InvalidAssign _, _) -> 11
      | (_, E.Unclassified _, _) -> 12
    in
    let errConstructor = function
      | (_, E.UndeclaredVar _, _) -> "UndeclaredVar"
      | (_, E.UndeclaredType _, _) -> "UndeclaredType"
      | (_, E.UnexpectedType _, _) -> "UnexpectedType"
      | (_, E.FuncNameAsVarName _, _) -> "FuncNameAsVarName"
      | (_, E.VarNameAsFuncName _, _) -> "VarNameAsFuncName"
      | (_, E.TooFewArgs _, _) -> "TooFewArgs"
      | (_, E.TooManyArgs _, _) -> "TooManyArgs"
      | (_, E.IncorrectType _, _) -> "IncorrectType"
      | (_, E.Duplicates _, _) -> "Duplicates"
      | (_, E.BreakNotAllowed, _) -> "BreakNotAllowed"
      | (_, E.Circularity, _) -> "Circularity"
      | (_, E.InvalidAssign _, _) -> "InvalidAssign"
      | (_, E.Unclassified _, _) -> "Unclassified"
    in
    let errorCmp e1 e2 = compare (errorAsInt e1) (errorAsInt e2)
    in
    let printErr (pos, err, comment) = match err with
      | E.UndeclaredVar s -> 
         say   (convertPos pos);
         sayln ("Undeclared name \"" ^ s ^ "\": " ^ comment)

      | E.UndeclaredType s ->
         say   (convertPos pos);
         sayln ("Undeclared type name \"" ^ s ^ "\": " ^ comment)

      | E.UnexpectedType (t1,t2) ->
         say   (convertPos pos); 
         sayln ("Unexpected type: " ^ comment);
         say   ("Expected: "); printIndent(out_chan, t1, 10);
         say   ("Found:    "); printIndent(out_chan, t2, 10)

      | E.FuncNameAsVarName nm ->
         say   (convertPos pos); 
         sayln ("Use of function name \"" ^ nm ^
                "\" as a variable name is not allowed. " ^ comment)

      | E.VarNameAsFuncName nm ->
         say   (convertPos pos); 
         sayln ("Use of variable name \"" ^ nm ^
                "\" as a function name is not allowed. " ^ comment)

      | E.TooFewArgs id ->
         say   (convertPos pos); 
         sayln ("Too few arguments for \"" ^ id ^ "\" " ^ comment)

      | E.TooManyArgs id ->
         say   (convertPos pos); 
         sayln ("Too many arguments for \"" ^ id ^ "\" " ^ comment)

      | E.IncorrectType ty->
         say   (convertPos pos); 
         sayln ("Incorrect type: " ^ comment);
         say   ("Type is: "); printIndent(out_chan, ty, 9)

      | E.Duplicates id ->
         say   (convertPos pos); 
         sayln ("The name \"" ^ id ^ "\" is used multiple times, " ^
                  comment)

      | E.BreakNotAllowed ->
         say   (convertPos pos); 
         sayln ("Break is not allowed in this context. " ^ comment)

      | E.Circularity ->
         say   (convertPos pos); 
         sayln ("Cycle in recursive type declarations. " ^ comment)

      | E.InvalidAssign id ->
         say   (convertPos pos); 
         sayln ("Assignment to loop control variable \"" ^ id ^ 
                "\" not allowed. " ^ comment)

      | E.Unclassified msg ->
         say   (convertPos pos); 
         sayln (msg ^ " " ^ comment)
    in
    let rec printErrs' = function
      | [] -> ()
      | e::es -> printErr e; sayln ""; printErrs' es
    in
    let rec printErrsConstructors = function
      | [] -> ()
      | e::es -> 
         say (errConstructor e); sayln ""; printErrsConstructors es
    in
    match errs with
    | [] -> sayln "No semantic errors."
    | _ -> if verbose 
           then printErrs' (List.sort errorCmp errs)
           else printErrsConstructors (List.sort errorCmp errs)

end

