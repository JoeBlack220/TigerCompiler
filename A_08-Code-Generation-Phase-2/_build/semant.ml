(* This file is an OCaml translation of code from a Standard ML file
   from Gopalan Nadathur for printing Tiger abstract syntax.  

   This OCaml translation is by Eric Van Wyk.
*)

module type SEMANT = sig
  type exp
  type transty
  type err
  val transProg : exp -> (transty * err list)
end

module Semant (Sy: Symbol.SYMBOL)
              (En: Env.ENV with type symbol = Sy.symbol  
                            and type 'a table = 'a Sy.table  )
              (Ab: Absyn.ABSYN with type symbol = Sy.symbol)
              (Er: Errormsg.ERRORMSG)
              (Tr: Translate.TRANSLATE with type label = En.label 
                                        and type access = En.access
                                        and type level = En.level)
              (Tm: Temp.TEMP with type label = Tr.label)
       : SEMANT with type exp = Ab.exp
                 and type transty = Tr.exp * En.ty
                 and type err = En.err
     = struct

type exp = Ab.exp
type err = En.err

type transty = Tr.exp * En.ty  (* <------------ Note this change *)

let semErrors : err list ref = ref []
let resetErrors () = semErrors := [] 
let report pos msg comment = semErrors := (pos,msg,comment) :: !semErrors


(* A bogus translation as fill-in for getting set up *)
let bogus = Tr.intExp 99999

let rec transProg (e: Ab.exp) : (transty * En.err list) =
  resetErrors ();
  let new_level = Tr.new_level { Tr.parent = Tr.outermost;
                                 name = Tm.named_label "$Main$$";
                                 formals = [] } 
  in
  let (main_tr, main_ty) = transExp En.base_venv En.base_tenv e None new_level
  in
  (* Next, we generate the $Main$$ fragment from the translation
     of the main program expression. *)
  let () = Tr.main new_level main_tr
    
  in ((main_tr, main_ty), !semErrors)

(* You need to provided a definition of transExp, transTy, transDec,
   and transDecs (by completing the skeletons below) that implement
   semantic analysis for Tiger as discussed in the book and in class.
   Note that type annontations are provided to help make the intention of
   the functions clear. 
*)
and transExp (venv: En.fnvar Sy.table) (tenv: En.ty Sy.table)
             (e: Ab.exp) (break: Tr.label option)
             (lev: Tr.level) : transty =

  (* Since venv and tenv do not change for nearly all expression
     this helper function is more convenient. *)
  let rec tExp = function
    | Ab.VarExp v -> transVar v
    | Ab.IntExp i -> (Tr.intExp i, En.INT)
    | Ab.StringExp (s, _) -> (Tr.string_exp s, En.STRING)
    | Ab.NilExp -> (Tr.nilExp (), En.NIL)  (* replace bogus, once plumbing is done *)
    | Ab.LetExp {decs; body; pos} ->
      let (venv', tenv', dexps) =
        List.fold_left (fun (venv, tenv, exps) d -> 
        let (venv1, tenv1, exps1) = 
        transDec venv tenv d break lev
        in
        (venv1, tenv1, exps@exps1)
        ) (venv, tenv, []) decs;
      in
      let (bexp, bty) = transExp venv' tenv' body break lev
      in
      ((Tr.letExp dexps bexp), bty) 
    | Ab.ArrayExp{typ; size; init; pos} ->
      begin match Sy.look tenv typ with
        | None ->
          report pos (En.UndeclaredVar (Sy.name typ)) "Undeclared arrray var."; 
          (bogus, En.ERROR)
        | Some t ->
          let tempTy = getActualTy t pos in
          begin match tempTy with
          | En.ARRAY(ty1, unique) ->
            let (sizeExp, sizeTy) = tExp size in
            let (initExp, initTy) = tExp init in
              checkTy En.INT sizeTy pos;
              checkTy ty1 initTy pos;
              ((Tr.arrayExp sizeExp initExp lev), (En.ARRAY(ty1, unique)))
          | _ -> 
            report pos (En.IncorrectType tempTy) "must be array type in array initialization."; 
            (bogus, En.ERROR)
          end
        end
    | Ab.AssignExp {var; exp; pos} ->
      begin match var with
      | Ab.SimpleVar(s, p) ->
        begin match (Sy.look venv s) with
        | Some En.VarEntry{access; ty; loopvar} ->
          if loopvar then 
          report pos (En.InvalidAssign (Sy.name s)) "You can't assign value to the loopvar."
        | Some (En.FunEntry {level; label; formals ; result})
            -> report p (En.FuncNameAsVarName (Sy.name s)) "Expect a var but get a function."
        | None 
            -> report p (En.UndeclaredVar (Sy.name s)) "Didn't find the var."
        end
      | _ -> ()
      end;
    let (vexp, vty) = transVar var in
    let (eexp, ety) = tExp exp in
      checkTy vty ety pos;
      ((Tr.assignExp vexp eexp), En.UNIT)

    | Ab.WhileExp{test; body; pos} ->
      let doneLabel = Tm.new_label() in
      let (testExp, testTy) = tExp test in
      let (bodyExp, bodyTy) = transExp venv tenv body (Some(doneLabel)) lev in
        checkTy En.INT testTy pos;
        checkTy En.UNIT bodyTy pos;
        ((Tr.whileExp testExp bodyExp doneLabel), En.UNIT)
    | Ab.BreakExp pos ->
      begin match break with
        | None -> report pos (En.BreakNotAllowed) "Break outside a loop";(bogus, En.UNIT)
        | _ -> ();(bogus, En.UNIT)
      end
    | Ab.RecordExp{fields; typ; pos} ->
      begin match Sy.look tenv typ with
      | None ->
        report pos (En.UndeclaredType (Sy.name typ)) "Undeclared arrray var."; 
        (bogus, En.ERROR)
      | Some t ->
        let tempTy = getActualTy t pos in
          begin match tempTy with
          | En.RECORD(fields1, unique) ->
            let flist = List.map (fun (symbol, e, pos) -> (symbol, (tExp e), pos)) fields in
            let fcomp = List.map (fun (symbol, (exp, ty), pos) -> (symbol,ty)) flist in
            let elist = List.map (fun (symbol, (exp, ty), pos) -> exp) flist in
            let fts = List.map (fun (symbol, (exp, ty), pos) -> (ty,pos)) flist in
            let fes = List.map (fun (symbol, (exp, ty), pos) -> exp) flist in
            if (List.length fcomp) = (List.length fields) then begin 
              List.iter2
                (fun (sym, typ) (sym1, typ1) ->
                  if sym = sym1
                  then begin 
                  checkTy typ typ1 pos;
                  end
                  else begin
                  report pos (En.Unclassified (Sy.name sym)) "Wrong record fields";
                  end) fcomp fields1;
              ((Tr.recExp elist), (En.RECORD (fields1, unique)));
            end
            else begin
              report pos (En.Unclassified (Sy.name typ)) "Wrong fields number.";
              (bogus, En.ERROR);
            end
            | _ ->          
              report pos (En.UndeclaredType (Sy.name typ)) "Undeclared record var.";
              (bogus, En.ERROR);
          end
      end

    | Ab.ForExp{var; escape; lo; hi; body; pos} ->
      let venv' = (Sy.enter venv var (En.VarEntry {access = (Tr.alloc_local lev !escape); ty = En.INT; loopvar = true})) in
      let breakpoint = Tm.new_label() in
      let (loExp, loTy) = tExp lo in
      let (hiExp, hiTy) = tExp hi in
        checkTy En.INT loTy pos;
        checkTy En.INT hiTy pos;
      let (bodyExp, bodyTy) = (transExp venv' tenv body (Some(breakpoint)) lev) in
        checkTy bodyTy En.UNIT pos;
        begin match Sy.look venv' var with
        | Some (En.VarEntry {access; ty ; loopvar}) -> ((Tr.forExp (Tr.simpleVar access lev) escape loExp hiExp bodyExp breakpoint), En.UNIT)
        end
    | Ab.CallExp{func; args; pos} ->
      begin match Sy.look venv func with
      | Some (En.FunEntry {level; label; formals; result}) ->
          let rec helper = function
            | (a::args,t::formals) -> let (argExp, argTy) = (tExp a) in 
                                        checkTy argTy t pos;
                                        argExp :: helper(args, formals)
            | ([],[])              -> []
            | (args,[])            -> report pos (En.TooManyArgs (Sy.name func)) "Too many arguments in a function.";
                                      []
            | ([],_)               -> report pos (En.TooFewArgs (Sy.name func)) "Too few arguments in a function.";
                                      []
          in
          let argsExp = helper(args,formals) in
            ((Tr.callExp lev level label argsExp), result)            
      | Some (En.VarEntry {access; ty ; loopvar})
            -> report pos (En.VarNameAsFuncName (Sy.name func)) "Expect a function but get a var."; 
               (bogus, En.ERROR)
      | None 
            -> report pos (En.UndeclaredVar (Sy.name func)) "Undeclared function name." ; 
               (bogus, En.ERROR)
      end
    | Ab.SeqExp exps ->
      let raw = List.map(fun (exp, pos) -> ((tExp exp), pos)) exps in
      let ckn = List.map(fun ((curExp,curTy), pos) -> (curTy, pos)) raw in
      let es = List.map(fun ((curExp,curTy),pos) -> curExp) raw in
      let rec checkSeq = function
      | []                    -> En.UNIT
      | [(expTy, pos)]        -> (checkNilSeq expTy pos);
                                 expTy
      | (expTy, pos)::rest    -> (checkNilSeq expTy pos); 
                                 checkSeq rest
      in
      let retTy = (checkSeq ckn) in
      ((Tr.seqExp es), retTy)

    | Ab.OpExp {left; oper; right; pos} ->
      begin
      let (le, lt) = tExp left in
      let (re, rt) = tExp right in
      match oper with
       | Ab.PlusOp ->
         checkTy lt En.INT pos;
         checkTy rt En.INT pos;
         ((Tr.plusExp le re), En.INT)
       | Ab.MinusOp ->
         checkTy lt En.INT pos;
         checkTy rt En.INT pos;
         ((Tr.minusExp le re), En.INT)
       | Ab.TimesOp ->
         checkTy lt En.INT pos;
         checkTy rt En.INT pos;
         ((Tr.timesExp le re), En.INT)
       | Ab.DivideOp ->
         checkTy lt En.INT pos;
         checkTy rt En.INT pos;
         ((Tr.divideExp le re), En.INT)

       | Ab.EqOp ->
         begin match lt with 
         | En.INT -> checkTy En.INT rt pos ; ((Tr.eqExp le re false), En.INT)
         | En.STRING -> checkTy En.STRING rt pos; ((Tr.eqExp le re true), En.INT)
         | En.ARRAY (t, u) -> checkTy (En.ARRAY(t, u)) rt pos; ((Tr.eqExp le re false), En.INT)
         | En.RECORD (fs, u) -> checkTy (En.RECORD(fs, u)) rt pos ; ((Tr.eqExp le re false), En.INT)
         | En.NIL -> checkTy lt rt pos ; ((Tr.eqExp le re false), En.INT)
         | _ -> report pos (En.UnexpectedType  (lt, rt)) "Don't support this kind of equal operation."; ((Tr.eqExp le re false), En.INT)
	end

       | Ab.NeqOp ->
         begin match lt with 
         | En.INT -> checkTy En.INT rt pos ; ((Tr.neqExp le re false), En.INT)
         | En.STRING -> checkTy En.STRING rt pos; ((Tr.neqExp le re true), En.INT)
         | En.ARRAY (t, u) -> checkTy (En.ARRAY(t, u)) rt pos; ((Tr.neqExp le re false), En.INT)
         | En.RECORD (fs, u) -> checkTy (En.RECORD(fs, u)) rt pos ; ((Tr.neqExp le re false), En.INT)
         | En.NIL -> checkTy lt rt pos ; ((Tr.eqExp le re false), En.INT)
         | _  -> report pos (En.UnexpectedType  (lt, rt)) "Don't support this kind of equal operation."; ((Tr.neqExp le re false), En.INT)
	end

       | Ab.LtOp ->
         begin match lt with 
         | En.INT -> checkTy En.INT rt pos; ((Tr.ltExp le re false), En.INT)
         | En.STRING -> checkTy En.STRING rt pos; ((Tr.ltExp le re true), En.INT)
         | _ -> report pos (En.UnexpectedType (lt, rt)) "Only support int and string type comparasion."; ((Tr.ltExp le re false), En.INT)
         end

       | Ab.LeOp ->
         begin match lt with 
         | En.INT -> checkTy En.INT rt pos; ((Tr.leExp le re false), En.INT)
         | En.STRING -> checkTy En.STRING rt pos; ((Tr.leExp le re true), En.INT)
         | _ -> report pos (En.UnexpectedType (lt, rt)) "Only support int and string type comparasion."; ((Tr.leExp le re false), En.INT)
         end

       | Ab.GtOp ->
         begin match lt with 
         | En.INT -> checkTy En.INT rt pos; ((Tr.gtExp le re false), En.INT)
         | En.STRING -> checkTy En.STRING rt pos; ((Tr.gtExp le re true), En.INT)
         | _ -> report pos (En.UnexpectedType (lt, rt)) "Only support int and string type comparasion."; ((Tr.gtExp le re false), En.INT)
         end

       | Ab.GeOp ->
         begin match lt with 
         | En.INT -> checkTy En.INT rt pos; ((Tr.geExp le re false), En.INT)
         | En.STRING -> checkTy En.STRING rt pos; ((Tr.geExp le re true), En.INT)
         | _ -> report pos (En.UnexpectedType (lt, rt)) "Only support int and string type comparasion."; ((Tr.geExp le re false), En.INT)
         end
      end

    | Ab.IfExp {test; then'; else'; pos} ->
      let (testExp, testTy) = tExp test in
      let (thenExp, thenTy) = tExp then' in
        checkTy testTy En.INT pos;
        begin match else' with
          | None -> checkTy En.UNIT thenTy pos;
                    ((Tr.ifThenExp testExp thenExp), En.UNIT)
          | Some e -> 
            let (elseExp, elseTy) = tExp e in
              checkTy thenTy elseTy pos;
              if (checkTyHelper thenTy elseTy) then
	        ((Tr.ifThenElseExp testExp thenExp elseExp), thenTy)
              else
                ((Tr.ifThenElseExp testExp thenExp elseExp), En.ERROR)
          end
  

    | _ -> Er.impossible ("Missing implelentation")
  
  and getIndex a x n =
    if (fst (List.nth a n)) = x then n
    else getIndex a x (n+1);
  and transVar : Ab.var -> transty  = function
      | Ab.SimpleVar (s, p ) ->
         (match Sy.look venv s with
          | Some (En.VarEntry {access; ty; loopvar}) 
            -> ((Tr.simpleVar access lev), getActualTy ty p)
          | Some (En.FunEntry {level; label; formals ; result})
            -> report p (En.FuncNameAsVarName (Sy.name s)) "Expect a var but get a function."; 
               (bogus, En.ERROR) 
          | None 
            -> report p (En.UndeclaredVar (Sy.name s)) "Didn't find the var." ; 
               (bogus, En.ERROR)  
         )
      | Ab.FieldVar (v, s, p) ->
        let (exp, ty) = transVar v in
        let ty = getActualTy ty p in
        begin match ty with 
        | En.RECORD (fields, _) ->
          begin
            try
              let field_ty = List.assoc s fields in
              let off_set = getIndex fields s 0 in
              ((Tr.fieVarExp exp off_set lev), getActualTy field_ty p)
            with Not_found -> report p (En.UndeclaredVar (Sy.name s)) "Undeclared Name";
            (bogus, En.ERROR)
          end
        | En.NIL -> (bogus, En.NIL)
        | _ -> report p (En.IncorrectType ty) "A record type is needed for field access.";
          (bogus, En.ERROR)
        end
      | Ab.SubscriptVar (v, e, p) ->
        let (exp, ty) = transVar v in
        begin match ty with
          | En.ARRAY (typ, _) ->
          let (exp1, ty1) = tExp e in
          let ty2 = getActualTy ty1 p in
            begin match ty2 with
            | En.INT -> ((Tr.subVarExp exp exp1 lev), typ)
            | _ -> report p (En.UnexpectedType (En.INT, ty1)) "The expersion of SubscriptVar need to be int.";
                   ((Tr.subVarExp exp exp1 lev), En.INT)
            end
          | _ -> report p (En.IncorrectType ty) "Something wrong with the array."; 
            (bogus, En.ERROR)
        end
         
      | _ -> Er.impossible ("Missing implementation")
  
  in tExp e


  and getActualTy (ty: En.ty) (pos: Ab.pos):En.ty = 
    begin match ty with
    |En.NAME (name, tyOpt) ->
      begin match !tyOpt with
      | None -> report pos (En.IncorrectType ty) "Unknown type.";
                En.ERROR
      | Some ty ->
          getActualTy ty pos
      end
    | _ -> ty
  end 


  and getSymTyHelper (sym: Ab.symbol) (tenv: En.ty Sy.table) (pos: Ab.pos) = 
    match Sy.look tenv sym with
    | Some ty -> ty
    | None    -> En.ERROR

  and getSymTy (sym: Ab.symbol) (tenv: En.ty Sy.table) (pos: Ab.pos) = 
    getActualTy (getSymTyHelper sym tenv pos) pos  

     
  and checkTyHelper ty1 ty2 = 
  match ty1, ty2 with
    | En.ERROR, _                                     -> true
    | _, En.ERROR                                     -> true
    | En.NAME (_, {contents=Some ty1}), ty2           -> checkTyHelper ty1 ty2
    | ty1, En.NAME (_, {contents=Some ty2})           -> checkTyHelper ty1 ty2
    | En.INT, En.INT                                  -> true
    | En.STRING, En.STRING                            -> true
    | En.NIL, En.NIL                                  -> true
    | En.UNIT, En.UNIT                                -> true
    | En.NIL, En.RECORD _                             -> true
    | En.RECORD (_, unique1), En.RECORD (_, unique2)  -> unique1 == unique2
    | En.ARRAY (_, unique1), En.ARRAY(_, unique2)     -> unique1 == unique2
    | En.RECORD _, En.NIL                             -> true
    | En.NIL, En.RECORD _                             -> true
    | _                                               -> false


  and checkTy ty1 ty2 pos = 
  if not (checkTyHelper ty1 ty2) then
     report pos (En.UnexpectedType (ty1,ty2)) "Found different type from expected type.";


  and checkNilSeq ty pos =
  if (ty == En.NIL) then
     report pos (En.IncorrectType En.NIL) "Expression in sequence cannot be incomplete record type.";

 and transFuncHead (venv: En.fnvar Sy.table) (tenv:En.ty Sy.table) (localVenv: En.fnvar Sy.table)  ({Ab.fnname; params; result; body; fnpos}) (lev: Tr.level) = 
   let rType = match result with
     | None -> En.UNIT
     | Some(sy, pos) -> 
     match Sy.look tenv sy with
     | Some(t) -> t
     | None -> report pos (En.UndeclaredType (Sy.name sy)) "The return type of the function doesn't exist.";
               En.ERROR
   in
   
   let fparam = 
     List.map
     (fun {Ab.fname; escape; typ; fpos} ->
     match Sy.look tenv typ with
     | Some(t) -> (fname, t)
     | None -> (fname, En.ERROR)) params
   in

   let esc = List.map (fun {Ab.fname; escape; typ; fpos} -> !escape) params
   in
   
   let tempLabel = (Tm.new_label()) in
  
   let (venvPlusF, localVenv') = 
     let t = (Sy.look localVenv fnname) in
     match t with
     | Some (En.FunEntry {level; label; formals ; result})
            -> report fnpos (En.Duplicates (Sy.name fnname)) "Already have a formal in this symbol.";
            let funE = (En.FunEntry {level = (Tr.new_level {parent = lev; name = tempLabel; formals = esc}) ;label = tempLabel; formals = (List.map snd fparam); result = rType}) in
               (Sy.enter venv fnname funE, 
                Sy.enter localVenv fnname funE)
     | _ ->
            let funE = (En.FunEntry {level = (Tr.new_level {parent = lev; name = tempLabel; formals = esc}) ;label = tempLabel; formals = (List.map snd fparam); result = rType}) in
               (Sy.enter venv fnname funE, 
                Sy.enter localVenv fnname funE)
     in
   venvPlusF,localVenv'

   
  and checkFuncVar (venv: En.fnvar Sy.table) (tenv:En.ty Sy.table) ({Ab.fnname; params; result; body; fnpos}) (break: Tr.label option) (lev: Tr.level)= 
  let rType = match result with
     | None -> En.UNIT
     | Some(sy, pos) -> 
     match Sy.look tenv sy with
     | Some(t) -> t
     | None -> En.ERROR
   in
  
  let t = (Sy.look venv fnname) in
    let (accList, curLevel) =  
      begin match t with
      | Some(En.FunEntry{level; label; formals; result}) -> (Tr.formals level, level)  
      end
    in
  
  let fparam = 
     List.map
     (fun {Ab.fname; escape; typ; fpos} ->
     match Sy.look tenv typ with
     | Some(t) -> (fname, t, escape)
     | None -> (fname, En.ERROR,escape)) params
   in

  let fparam2 = 
     List.map2(fun (fname, t, escape) acess ->
       (fname, t, escape, acess)) fparam accList
   in

   let venvPlusP = 
     let checkVarEnv : (En.fnvar Sy.table) = Sy.empty in
     List.fold_left
       (fun env' (name, typ, esc, acc) ->
          let t = (Sy.look env' name) in
          match t with
          | Some (En.VarEntry {access; ty; loopvar}) 
            -> report fnpos (En.Duplicates (Sy.name name)) "Already have a formal in this symbol.";
               Sy.enter env' name (En.VarEntry{access = acc; ty = typ; loopvar = false})
          | Some (En.FunEntry {level; label; formals ; result})
            -> report fnpos (En.FuncNameAsVarName (Sy.name name)) "Expect a var but get a function." ;
               Sy.enter env' name (En.VarEntry{access = acc; ty = typ; loopvar = false})
          | None 
            -> Sy.enter env' name (En.VarEntry{access = acc; ty = typ; loopvar = false})) checkVarEnv fparam2;
     List.fold_left
       (fun env' (name, typ, esc, acc) ->
         Sy.enter env' name (En.VarEntry{access = acc; ty = typ; loopvar = false})) venv fparam2;
   in
   
   let (bexp, bodyT)  =
     transExp venvPlusP tenv body break curLevel 
   in
     (Tr.procEntryExit curLevel bexp);
     if not (checkTyHelper bodyT rType) then
     report fnpos (En.UnexpectedType (bodyT, rType)) "Function has different body type and return type."

  and transTy  (tenv: En.ty Sy.table) (ty: Ab.ty) : En.ty =
    match ty with
    | Ab.NameTy (s, p) ->
      begin match Sy.look tenv s with
        | Some(ty) -> ty
        | _ -> report p (En.UndeclaredType (Sy.name s)) "Undeclared type.";
               En.ERROR
      end
    | Ab.RecordTy (fields) ->
        let fields1 = List.map
        (fun {Ab.fname; Ab.escape; typ; fpos} -> 
         begin match Sy.look tenv typ with
           | Some(t) -> fname, t
           | _       -> report fpos (UndeclaredType (Sy.name typ)) "Undeclared type name";
                        fname, En.ERROR  
           end) fields in
        En.RECORD(fields1, ref())
    | Ab.ArrayTy (s, p) ->
      begin match Sy.look tenv s with
        | Some(ty) -> En.ARRAY(ty, ref())
        | _ -> report p (En.UndeclaredType (Sy.name s)) "Undeclared arrray type.";
               En.ERROR
     | _ -> Er.impossible ("Missing implelentation")
      end
     
 
  and transDec (venv: En.fnvar Sy.table) (tenv: En.ty Sy.table)
               (d: Ab.dec) (break: Tr.label option) (lev: Tr.level)
    = begin match d with
    | Ab.VarDec{name; escape; typ; init; pos} -> 
      let (exp, t) = transExp venv tenv init break lev in
      let acc = Tr.alloc_local lev !escape in
      let varExp = (Tr.simpleVar acc lev) in  
        begin match typ with
        | Some (symbol, pos) -> 
          begin match Sy.look tenv symbol with
          | Some(dty) ->
            checkTy t dty pos;
            (Sy.enter venv name (En.VarEntry{access = acc; ty = dty; loopvar = false}), tenv, [(Tr.assignExp varExp exp)])
          | None -> 
            report pos (En.UndeclaredType (Sy.name symbol)) "Didn't find the type." ;
            (Sy.enter venv name (En.VarEntry{access = acc; ty = t; loopvar = false}), tenv, [])
          end
        | None -> 
          if(t == En.NIL) then
            report pos (En.IncorrectType En.NIL) "Nil type not allowed in variable declaration." 
          else if(t == En.UNIT) then
            report pos (En.IncorrectType En.UNIT) "Unit type not allowed in variable declaration.";
            (Sy.enter venv name (En.VarEntry{access = acc; ty = t; loopvar = false}), tenv, [(Tr.assignExp varExp exp)])
        end
   | Ab.TypeDec types ->
     let localTenv = Sy.empty in
     let (tenv', localTenv') = 
       List.fold_left
       (fun (tenv',localTenv') {Ab.tname; ty; tpos} -> 
       let t = (Sy.look localTenv' tname) in
          match t with
          | Some sym
            -> report tpos (En.Duplicates (Sy.name tname)) "Already have this type declaration.";
               (Sy.enter tenv' tname (En.NAME (tname, ref None)), Sy.enter localTenv' tname (En.NAME (tname, ref None)))
          | None 
            -> (Sy.enter tenv' tname (En.NAME (tname, ref None)), Sy.enter localTenv' tname (En.NAME (tname, ref None))))
               (tenv, localTenv) types
     in
       List.iter 
       (fun ({Ab.tname; ty; tpos}) -> 
         let tempTy = transTy tenv' ty in
         begin match getSymTyHelper tname tenv' tpos with
         | En.NAME (_, tyOpt) -> tyOpt := Some tempTy;
         | _ -> ()
         end) types;
         checkCycleHelper types tenv' ;
       (venv, tenv', [])
   | Ab.FunctionDec fundecs ->
     let localVenv = Sy.empty in
     let (venvWithFunHead,_) = 
     List.fold_left
       (fun (venv',localVenv') dec -> transFuncHead venv' tenv localVenv' dec lev) (venv, localVenv) fundecs
     in 
     List.iter (fun dec -> checkFuncVar venvWithFunHead tenv dec break lev) fundecs;
     (venvWithFunHead, tenv, [])

   end

  and checkCycleHelper types tenv' = 
  let flag = false in
  List.fold_left 
       (fun flag' ({Ab.tname; ty; tpos}) -> 
         if not flag' then
         begin match getSymTyHelper tname tenv' tpos with
         | En.NAME (_, tyOpt) as t->
           begin match !tyOpt with
           | Some tempTy -> checkCycle tempTy [tname] tpos
           | _ -> false
           end
         | _ -> false
         end
         else
         true) flag types;
 
  and checkCycle ty (symList :Ab.symbol list) (pos:Ab.pos) =
  begin match ty with
  | En.NAME (sym2, tyOpt) ->
    if (checkList sym2 symList) then begin
        report pos (En.Circularity) "Detected type circulation.";
        true
    end
    else
      begin match !tyOpt with
      | Some (En.NAME(sym3, tyOpt) as t)-> 
        checkCycle t (sym2::symList) pos
      | _ -> false
    end
  | _ -> false
  end

  and checkList (sym: Ab.symbol) (symList :Ab.symbol list) =
    match symList with
    | hd::tl -> if ((Sy.name hd) = (Sy.name sym)) then
                true
                else checkList sym tl
    | []     -> false

end


