(* This file is an OCaml translation of code from a Standard ML file
   from Gopalan Nadathur for printing Tiger abstract syntax.  

   This OCaml translation is by Eric Van Wyk.  Some modification to the
   module structure of the files was made to accommodate some pecularities
   of OCaml-Yacc and limitations for generationg modules that do not
   hinder the Standard ML Yacc parser generator.
*)

module Semant (Sy: Symbol.SYMBOL)
              (En: Env.ENV with type symbol = Sy.symbol  
                           and  type 'a table = 'a Sy.table  )
              (Ab: Absyn.ABSYN with type symbol = Sy.symbol)
              (Er: Errormsg.ERRORMSG)
     = struct

type transty = En.ty
type err = En.err

let semErrors : err list ref = ref []
let resetErrors () = semErrors := [] 

let report pos msg comment = semErrors := (pos,msg,comment) :: !semErrors

let rec transProg (e: Ab.exp) : transty * En.err list =
  resetErrors ();
  let ty = transExp En.base_venv En.base_tenv e false
  in (ty, !semErrors)

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
  
and transExp (venv: En.fnvar Sy.table) (tenv: En.ty Sy.table)
             (e: Ab.exp) (inloop: bool)
             : transty =
  (* since venv and tenv do not change for nearly all expression
       a helper function is more convenient *)

  let rec tExp = function
    | Ab.VarExp v -> transVar v

    | Ab.IntExp _ -> En.INT

    | Ab.StringExp _ -> En.STRING

    | Ab.NilExp -> En.NIL

    | Ab.IfExp {test; then'; else'; pos} ->
      if (checkTyHelper (tExp test) En.INT) then begin
        match else' with
        | None ->           
          if (checkTyHelper (tExp then') En.UNIT) then
            tExp then'
          else 
            begin
            report pos (En.UnexpectedType ((tExp then'), En.UNIT)) "The type of then branch should be unit."; 
            En.UNIT
            end
        | Some else' ->
          if (checkTyHelper (tExp then') (tExp else')) then
            tExp then'
          else 
            begin
            report pos (En.UnexpectedType ((tExp then'), (tExp else'))) "The type of both branches should be the same."; 
            En.ERROR
            end
      end
      else begin
        report pos (En.IncorrectType (tExp test)) "The test field of If expression need to be int.";
        En.ERROR;
      end

    | Ab.LetExp{decs; body; pos} ->
       (* since the environments may change, the 
          recursive call is to transExp and not tExp *)
       let (venv', tenv') = transDecs venv tenv decs in
       transExp venv' tenv' body false

    | Ab.ArrayExp {typ; size; init; pos} ->
      if (checkTyHelper (tExp size) En.INT) then begin
        let tElem = tExp init in
        begin
          match Sy.look tenv typ with
          | Some (t) ->
            let tempTy = getActualTy t pos in
            begin match tempTy with 
            | En.ARRAY(ty1, unique) ->
              checkTy tElem ty1 pos;
              tempTy
            | _ ->
              report pos (En.IncorrectType tElem) "must be array type in array initialization."; 
              En.ERROR
            end
          | None ->
            report pos (En.UndeclaredVar (Sy.name typ)) "Undeclared arrray var."; 
            En.ERROR
       end
     end
     else begin
       report pos (En.UnexpectedType (En.INT,(tExp size))) "The size field of array expression need to be int.";
       let tElem = tExp init in
        begin
          match Sy.look tenv typ with
          | Some (t) ->
            let tempTy = getActualTy t pos in
            begin match tempTy with 
            | En.ARRAY(ty1, unique) ->
              checkTy tElem ty1 pos;
              tempTy
            | _ ->
              En.ERROR
            end
          | None ->
            En.ERROR
       end
     end 

    | Ab.AssignExp {var; exp; pos} ->
      begin match var with
      | Ab.SimpleVar(s, p) ->
        begin match (Sy.look venv s) with
        | Some En.VarEntry{ty; loopvar} ->
          if loopvar then 
          report pos (En.InvalidAssign (Sy.name s)) "You can't assign value to the loopvar."
        | Some (En.FunEntry {formals ; result})
            -> report p (En.FuncNameAsVarName (Sy.name s)) "Expect a var but get a function."
        | None 
            -> report p (En.UndeclaredVar (Sy.name s)) "Didn't find the var."
        end
      | _ -> ()
      end;
      if (checkTyHelper (transVar var) (tExp exp)) then
        En.UNIT
      else begin
        report pos (En.UnexpectedType ((transVar var), (tExp exp))) "The types of var and expression in an assignexpression need to be the same.";
        En.UNIT
      end

    | Ab.WhileExp {test; body; pos} ->
      if not (checkTyHelper (tExp test) En.INT) then
        report pos (En.UnexpectedType (En.INT,(tExp test))) "The test field of while expression need to be int.";
      if not (checkTyHelper (transExp venv tenv body true) En.UNIT) then
        report pos (En.UnexpectedType (En.UNIT, (transExp venv tenv body true))) "The body expression of while expression need to be unit.";
      En.UNIT

    | Ab.BreakExp pos ->
      if not inloop then begin 
      report pos (En.BreakNotAllowed) "Break outside a loop";
      En.ERROR;
      end
      else
      En.UNIT

    | Ab.RecordExp{fields; typ; pos} ->
      let tempTy = Sy.look tenv typ in      
      begin match tempTy with
        
        | Some(ty) ->
          let tempTy2 = getActualTy ty pos in
          begin match tempTy2 with
          | En.RECORD(fields1, unique) ->
            List.iter2
              (fun (sym, typ) (sym1, typ1) ->
                if sym = sym1
                then begin 
                checkTy typ typ1 pos;
                end
                else begin
                report pos (En.Unclassified (Sy.name sym)) "Wrong record fields";
                end) (fieldHelper fields) fields1;
            En.RECORD (fields1, unique)
         | _ ->            
          report pos (En.UndeclaredType (Sy.name typ)) "Undeclared record var.";
          En.ERROR
          end
        | None ->
          report pos (En.UndeclaredType (Sy.name typ)) "Undeclared record var.";
          En.ERROR
        end
        
    | Ab.ForExp {var; escape; lo; hi; body; pos} ->
          if not (checkTyHelper (tExp lo) En.INT) then 
          report pos (En.UnexpectedType (En.INT, (tExp lo))) "The lo field of For expression need to be int.";
          if not (checkTyHelper (tExp hi) En.INT) then
          report pos (En.UnexpectedType (En.INT, (tExp hi))) "The hi field of For expression need to be int.";
          let venv' =
            Sy.enter venv var (En.VarEntry{ty = En.INT; loopvar = true})
            in
            if not (checkTyHelper (transExp venv' tenv body true) En.UNIT) then 
             report pos (En.UnexpectedType (En.UNIT, (transExp venv' tenv body true))) "The body expression of while expression need to be unit.";
            En.UNIT
    | Ab.CallExp {func; args; pos} ->
      let fEntry = Sy.look venv func in
      begin match fEntry with
      | Some (En.FunEntry {formals; result}) ->
            let rec helper = function
              | (a::args,t::formals) -> checkTy (tExp a) t pos;
                                        helper(args, formals)
              | ([],[])              -> ()
              | (args,[])            -> report pos (En.TooManyArgs (Sy.name func)) "Too many arguments in a function."
              | ([],_)               -> report pos (En.TooFewArgs (Sy.name func)) "Too few arguments in a function."
            in
            helper(args,formals);
            result
            
      | Some (En.VarEntry {ty ; loopvar})
            -> report pos (En.VarNameAsFuncName (Sy.name func)) "Expect a function but get a var."; 
               En.ERROR
      | None 
            -> report pos (En.UndeclaredVar (Sy.name func)) "Undeclared function name." ; 
               En.ERROR
      end

    | Ab.SeqExp exps ->
      let rec checkSeq = function
      | []                  -> En.UNIT
      | [(exp, pos)]        -> let tempTy = tExp exp in
                               (checkNilSeq tempTy pos);
                               tempTy
      | (exp, pos)::rest    -> let tempTy = tExp exp in
                               (checkNilSeq tempTy pos);
                               ignore (tempTy); 
                               checkSeq rest
      in
      checkSeq exps;

    | Ab.OpExp {left; oper; right; pos} ->
      begin
      match oper with
       | Ab.PlusOp | Ab.MinusOp | Ab.TimesOp | Ab.DivideOp ->
         if not (checkTyHelper (tExp left) En.INT) then begin
           report pos (En.UnexpectedType ((tExp left),(tExp right))) "The left expression of operate expression need to be int.";
           En.INT
         end
         else if not (checkTyHelper (tExp right) En.INT) then begin
           report pos (En.UnexpectedType  ((tExp left),(tExp right))) "The right expression of operate expression need to be int.";
           En.INT
         end
         else En.INT;
       | Ab.EqOp | Ab.NeqOp ->
         if not (checkTyHelper (tExp left) (tExp right)) then begin
           report pos (En.UnexpectedType  ((tExp left),(tExp right))) "The left and the right expression must be the same type.";
           En.INT
         end
         else En.INT;

       | Ab.LtOp | Ab.LeOp | Ab.GtOp | Ab.GeOp ->
         if not ((checkTyHelper (tExp left) En.INT) || (checkTyHelper (tExp left) En.STRING)) then begin
           report pos (En.IncorrectType (tExp left)) "The left expression of operate expression need to be int or string.";
           En.INT
         end
         else if  not (checkTyHelper (tExp left) (tExp right)) then begin
           report pos (En.UnexpectedType ((tExp right),(tExp right))) "The right expression and the left expression must match in type.";
           En.INT
         end
         else En.INT;
      end
        
        


  and fieldHelper fields =
    match fields with
    | []                                -> []
    | (symbol, exp, pos) :: []          -> (symbol, (tExp exp)) :: []
    | (symbol, exp, pos) :: res          -> (symbol, (tExp exp)) :: fieldHelper res 

  and transVar : Ab.var -> En.ty  = function
      | Ab.SimpleVar (s, p ) ->
         (match Sy.look venv s with
          | Some (En.VarEntry {ty; loopvar}) 
            -> getActualTy ty p
          | Some (En.FunEntry {formals ; result})
            -> report p (En.FuncNameAsVarName (Sy.name s)) "Expect a var but get a function."; 
               En.ERROR (* Fix this *)
          | None 
            -> report p (En.UndeclaredVar (Sy.name s)) "Didn't find the var." ; 
               En.ERROR (* Fix this *)
         )
      | Ab.FieldVar (v, s, p) ->
        let ty = getActualTy (transVar v) p in
        begin match ty with 
        | En.RECORD (fields, _) ->
          begin
            try
              let field_ty = List.assoc s fields in
              getActualTy field_ty p
            with Not_found -> report p (En.UndeclaredVar (Sy.name s)) "Undeclared Name";
            En.ERROR
          end
        | _ -> report p (En.IncorrectType ty) "A record type is needed for field access.";
          En.ERROR
        end
      | Ab.SubscriptVar (v, e, p) ->
        let ty = transVar v in
        begin match ty with
          | En.ARRAY (typ, _) ->
          if (checkTyHelper (tExp e) En.INT) then
          typ
          else begin 
           report p (En.UnexpectedType (En.INT, (tExp e))) "The expersion of SubscriptVar need to be int.";
           En.INT
          end
          | _ -> report p (En.IncorrectType ty) "Something wrong with the array."; 
            En.ERROR
        end
         
      | _ -> Er.impossible ("Missing implementation")

  in tExp e

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

  and transTy  (tenv: En.ty Sy.table) (ty: Ab.ty) : En.ty =
    match ty with
    | Ab.NameTy (s, p) ->
      begin match Sy.look tenv s with
        | Some(ty) -> 
                      ty
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
  
  and transFuncHead (venv: En.fnvar Sy.table) (tenv:En.ty Sy.table) (localVenv: En.fnvar Sy.table)  ({Ab.fnname; params; result; body; fnpos}) = 
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
  
   let (venvPlusF, localVenv') = 
     let t = (Sy.look localVenv fnname) in
     match t with
     | Some (En.FunEntry {formals ; result})
            -> report fnpos (En.Duplicates (Sy.name fnname)) "Already have a formal in this symbol.";
               (Sy.enter venv fnname (En.FunEntry {formals = (List.map snd fparam); result = rType}), 
                Sy.enter localVenv fnname (En.FunEntry {formals = (List.map snd fparam); result = rType}))
     | _ ->
           (Sy.enter venv fnname (En.FunEntry {formals = (List.map snd fparam); result = rType}), 
           Sy.enter localVenv fnname (En.FunEntry {formals = (List.map snd fparam); result = rType}))
     in
   venvPlusF,localVenv'

   
  and checkFuncVar (venv: En.fnvar Sy.table) (tenv:En.ty Sy.table) ({Ab.fnname; params; result; body; fnpos}) = 
  let rType = match result with
     | None -> En.UNIT
     | Some(sy, pos) -> 
     match Sy.look tenv sy with
     | Some(t) -> t
     | None -> En.ERROR
   in
  
  let fparam = 
     List.map
     (fun {Ab.fname; escape; typ; fpos} ->
     match Sy.look tenv typ with
     | Some(t) -> (fname, t)
     | None -> (fname, En.ERROR)) params
   in

   let venvPlusP = 
     let checkVarEnv : (En.fnvar Sy.table) = Sy.empty in
     List.fold_left
       (fun env' (name, typ) ->
          let t = (Sy.look env' name) in
          match t with
          | Some (En.VarEntry {ty; loopvar}) 
            -> report fnpos (En.Duplicates (Sy.name name)) "Already have a formal in this symbol.";
               Sy.enter env' name (En.VarEntry{ty = typ; loopvar = false})
          | Some (En.FunEntry {formals ; result})
            -> report fnpos (En.FuncNameAsVarName (Sy.name name)) "Expect a var but get a function." ;
               Sy.enter env' name (En.VarEntry{ty = typ; loopvar = false})
          | None 
            -> Sy.enter env' name (En.VarEntry{ty = typ; loopvar = false})) checkVarEnv fparam;
     List.fold_left
       (fun env' (name, typ) ->
         Sy.enter env' name (En.VarEntry{ty = typ; loopvar = false})) venv fparam;
   in
   
   let bodyT =
     transExp venvPlusP tenv body false
   in

     if not (checkTyHelper bodyT rType) then
     report fnpos (En.UnexpectedType (bodyT, rType)) "Function has different body type and return type."
     
 
  and transDec (venv: En.fnvar Sy.table) (tenv: En.ty Sy.table)
               (d: Ab.dec) 
      : (En.fnvar Sy.table * En.ty Sy.table )
    = begin match d with
    | Ab.VarDec{name; escape; typ; init; pos} -> 
      let tInit = transExp venv tenv init false in
      let tVar = 
        begin match typ with
        | Some (symbol, pos) -> 
          let Some(t) = (Sy.look tenv symbol) in
          if(checkTyHelper t tInit) then begin
            t
          end                   
          else begin
          report pos (En.UnexpectedType (t, tInit)) "Inconsistent types occur in var declaration.";
          t
          end
        | None -> 
          if(tInit == En.NIL) then
            report pos (En.IncorrectType En.NIL) "Nil type not allowed in variable declaration." 
          else if(tInit == En.UNIT) then
            report pos (En.IncorrectType En.UNIT) "Unit type not allowed in variable declaration.";
          tInit
          end
          in
        Sy.enter venv name (En.VarEntry{ty = tVar; loopvar = false}), tenv
   | Ab.TypeDec types ->
     let localTenv = Sy.empty in
     let (tenv', localTenv') = 
       List.fold_left
       (fun (tenv',localTenv') {Ab.tname; ty; tpos} -> 
       let t = (Sy.look localTenv' tname) in
          match t with
          | Some sym
            -> report tpos (En.Duplicates (Sy.name tname)) "Already have this type declaration.";
               (Sy.enter tenv' tname (En.NAME (tname, ref None)),Sy.enter localTenv' tname (En.NAME (tname, ref None)))
          | None 
            -> (Sy.enter tenv' tname (En.NAME (tname, ref None)),Sy.enter localTenv' tname (En.NAME (tname, ref None))))
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
       venv,tenv'
   | Ab.FunctionDec fundecs ->
     let localVenv = Sy.empty in
     let (venvWithFunHead,_) = 
     List.fold_left
       (fun (venv',localVenv') dec -> transFuncHead venv' tenv localVenv' dec) (venv, localVenv) fundecs
     in 
     List.iter (fun dec -> checkFuncVar venvWithFunHead tenv dec) fundecs;
     venvWithFunHead, tenv

   | _ -> Er.impossible ("Missing implelentation")
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


  and transDecs (venv: En.fnvar Sy.table) (tenv: En.ty Sy.table) 
                (ds: Ab.dec list) 
      : (En.fnvar Sy.table * En.ty Sy.table)
    = 
      List.fold_left
      (fun (venv, tenv) dec ->transDec venv tenv dec) (venv, tenv) ds


end

