
(* The following type declarations are OCaml versions of what is
   provided in Appel's textbook Modern Compiler Implementation in
   ML.  
*)

type id = string

type binop = Plus | Minus | Times | Div

type stm = CompoundStm of stm * stm
	 | AssignStm of id * exp
	 | PrintStm of exp list
                     
 and exp = IdExp of id
	 | NumExp of int
         | OpExp of exp * binop * exp
         | EseqExp of stm * exp

type table = (id * int) list

(* The program provided in your text.
 *)
let prog1 = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp "a"; OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))


(* Prog2 is a simple sequence of assignments, ending with a print.
 *)
let prog2 = 
 CompoundStm( AssignStm("x", OpExp(NumExp 2, Times, NumExp 3)),
  CompoundStm( AssignStm("y", OpExp(IdExp "x", Plus, NumExp 4)),
   CompoundStm( AssignStm("x", OpExp(IdExp "x", Plus, IdExp "y")),
    CompoundStm( AssignStm("y", OpExp(IdExp "y", Minus, NumExp 3)),
     PrintStm [IdExp "x"; IdExp "y"]
    ) ) ) )


(* Prog3 is semantically the same as prog2, but organizes the four 
   assignments in a balances tree instead of an unbalanced one as in 
   prog2.  The results of interp should be identical.
 *)
let prog3 = 
 CompoundStm( 
   CompoundStm( 
     CompoundStm( AssignStm("x", OpExp(NumExp 2, Times, NumExp 3)),
                  AssignStm("y", OpExp(IdExp "x", Plus, NumExp 4)) ),
     CompoundStm( AssignStm("x", OpExp(IdExp "x", Plus, IdExp "y")),
                  AssignStm("y", OpExp(IdExp "y", Minus, NumExp 3))) ),
   PrintStm [IdExp "x"; IdExp "y"]
  )

(* An example with a print inside a print.  Eric's solution first
   evaluates all the expressions in the PrintStm and then prints them.
   It is also reasonable to print each expression after it has been
   evaluated but before the next one is evaluated.  This is not so
   important and is not considered in the grading of this assignment
   since our only goal is to get familiar with OCaml and some basic
   ideas from language implementation.  
*)
let prog4 =
  CompoundStm(AssignStm("a",NumExp 0),
              PrintStm([NumExp 5;
                        EseqExp(PrintStm([IdExp "a"; NumExp 3]),
                                OpExp(IdExp "a", Plus, NumExp 1));
                        EseqExp(AssignStm("a", NumExp 7), IdExp "a")
                      ]))


(* Complete the implementations of these two function. *)

let rec maxargs (s: stm) : int =
  match s with
  | CompoundStm (s1, s2) -> max (maxargs s1) (maxargs s2)
  | AssignStm (i1, e1) -> proexp e1
  | PrintStm l -> proprint l

and proexp (e: exp) : int =
  match e with
  | EseqExp (s1, e1) -> (maxargs s1) + (proexp e1)
  | _ -> 0

and prolist (l: exp list) : int =
  match l with
  | [] -> 0
  | hd :: tl -> max (proexp hd) (prolist tl)

and proprint (l: exp list) : int = 
  match l with
  | [] -> 0
  | hd :: tl -> max (proexp hd) (1+ (proprint tl))  

let rec interp (prog:stm) : table =
  interpstm prog []

and interpstm (s:stm)  (t:table) : table =
  match s with
  | CompoundStm (s1,s2) -> interpstm s2 (interpstm s1 t)
  | AssignStm (i, e) -> 
                  let (r,t1) = interpexp e t in 
                  (i,r)::t1
  | PrintStm l -> let (a,b) = interplist l t [] in
                  b

and interplist (l:exp list) (t:table) (il:int list): (int list) * table = 
  match l with
  | [] -> printhelper il;
          (il,t)
  | hd::tl -> let (a,b) = (interpexp hd t) in
              interplist tl b (il@[a])

and printhelper (il: int list) =
  match il with
  | hd::[] -> Printf.printf "%d\n" hd
  | hd::tl -> Printf.printf "%d, " hd;
              printhelper tl

and interpexp (e:exp) (t:table) : int*table =
  match e with
  | IdExp i -> (matchi i t,t)  
  | NumExp n -> (n, t)
  | OpExp (e1,b1,e2) -> (eval e1 b1 e2 t) 
  | EseqExp (s,e) -> interpexp e (interpstm s t)

and matchi (i:id) (t:table) : int = 
  match t with
  | [] -> 0
  | hd::tl -> let (a,b) = hd in
              if a = i then b else (matchi i tl)
 
and eval (e1:exp) (b:binop) (e2:exp) (t:table) : int*table =
  let (v1,t2) = (interpexp e1 t) in
    let (v2,t3) = (interpexp e2 t) in
      match b with
      | Plus -> (v1+v2,t3)
      | Minus -> (v1-v2,t3)
      | Times -> (v1*v2,t3)
      | Div -> (v1/v2,t3)

(* Keep these testing declarations as the last thing in your file so
   that you see them when you load the file into `ocaml` or
   `utop`. 
*)

let () = 
  print_string "Testing maxargs ...\n" ;
  try
    assert (maxargs prog1 = 2);
    assert (maxargs prog2 = 2);
    assert (maxargs prog3 = 2);
    assert (maxargs prog4 = 3);
    print_string "... tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "Assert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n"
    in print_string msg


let () = 
  print_string "Testing interp ...\n" ;
  try
    assert (interp prog1 = [("b", 80); ("a", 8)]);
    assert (interp prog2 = [("y", 7); ("x", 16); ("y", 10); ("x", 6)]);
    assert (interp prog3 = [("y", 7); ("x", 16); ("y", 10); ("x", 6)]);
    assert (interp prog4 = [("a", 7); ("a", 0)]);
    print_string "... tests passed.\n"
  with
    Assert_failure (file, line, column) -> 
    let msg = "Assert failed on line " ^ string_of_int line ^ 
                ", column " ^ string_of_int column ^ "\n\n"
    in print_string msg
