(* Standar ML code courtesy of Gopalan Nadathur.
   Translation to OCaml by Eric Van Wyk.
 *)

module type FINDESCAPES = sig
  type exp
  val find_escapes: exp -> unit
end

module FindEscapes (Ab: Absyn.ABSYN)
                   (Sy: Symbol.SYMBOL with type symbol = Ab.symbol)
                 : (FINDESCAPES with type exp = Ab.exp)
  = struct

  type exp = Ab.exp
  type depth = int
  type esc_env = (depth * bool ref) Sy.table
  
  (* Implement `findescapes` here to fill in the escape slot for
     variables as discussed in class. *)

  let find_escapes e =
    let eenv = Sy.empty in

      let rec traExp (eenv: esc_env) (d: depth) (expr: exp) =
        begin match expr with
        | Ab.VarExp v -> traVar eenv d v

        | Ab.NilExp | Ab.IntExp _  | Ab.StringExp (_, _) -> ()

        | Ab.CallExp {func: Ab.symbol ; args: exp list; pos: Ab.pos} ->
          List.iter (traExp eenv d) args

        | Ab.OpExp {left; oper; right; pos}->
          traExp eenv d left;
          traExp eenv d right

        | Ab.RecordExp {fields; typ; pos} ->
          List.iter (fun(_, exp, _) -> traExp eenv d exp) fields

        | Ab.SeqExp(exps) ->
          List.iter (fun(exp, _) -> traExp eenv d exp) exps

        | Ab.AssignExp {var; exp; pos} ->
          traVar eenv d var;
          traExp eenv d exp

        | Ab. IfExp{test; then'; else'; pos} ->
          begin
            traExp eenv d test;
            traExp eenv d then';
            begin match else' with
             | Some(elseExp) -> traExp eenv d elseExp
             | None -> ()
            end
          end

         | Ab.WhileExp{test; body; pos} ->
           traExp eenv d test;
           traExp eenv d body

         | Ab.ForExp{var; escape; lo; hi; body; pos} ->
           let newEenv = Sy.enter eenv var (d, escape) in
             escape := false;
             traExp eenv d lo;
             traExp eenv d hi;
             traExp newEenv d body
 
         | Ab.BreakExp _ -> ()

         | Ab.LetExp {decs; body; pos} ->
           let newEenv = traDecs eenv d decs in
             traExp newEenv d body
        
         | Ab.ArrayExp{typ; size; init; pos} ->
           traExp eenv d size;
           traExp eenv d init

        end

      and traVar (eenv: esc_env) (d: depth) (v: Ab.var) =
        begin match v with
        | Ab.SimpleVar(sym, pos) ->
          begin match (Sy.look eenv sym) with
          | Some(d', esc) ->
            if (d > d') then esc := true else ()

          | _ -> ()
          end

        | Ab.FieldVar(var, symbol, pos) ->
          traVar eenv d var
        
        | Ab.SubscriptVar(var, exp, pos) ->
          traVar eenv d var;
          traExp eenv d exp    

        end
  
      and traDecs (eenv: esc_env) (d: depth) (decs: Ab.dec list) : esc_env =
        let doDec eenv dec= 
          begin match dec with
            | Ab.FunctionDec (fundecs) ->
              List.fold_left
              (fun env {Ab.fnname; params; result; body; fnpos} ->
                 let newEenv = (List.fold_left (fun env {Ab.fname; escape; typ; fpos} ->
                   escape := false;
                   Sy.enter env fname (d+1, escape)) eenv params)
                 in 
                   traExp newEenv (d+1) body;
                   env) eenv fundecs

            | Ab.VarDec{name; escape; typ; init; pos} ->
              escape := false;
              traExp eenv d init;
              Sy.enter eenv name (d, escape)
            | Ab.TypeDec _ -> eenv 
          end
        in 
          List.fold_left doDec eenv decs
  
  in
  
  traExp eenv 0 e

end

