(* Copyright (C) 2007 Gopalan Nadathur 

   Translated to OCaml by Eric Van Wyk
*) 

open Graph

module type REGALLOC = sig

  type instr
  type temp
  type frame

  val regalloc: instr list -> temp list -> frame -> 
                instr list * (temp -> temp)
end

module RegAlloc (Tm: Temp.TEMP)
                (Ig: Igraph.IGRAPH with type temp = Tm.temp)
                (Cl: Color.COLOR with type temp = Tm.temp
                                  and type temp = Ig.temp
                                  and type igraph = Ig.igraph)
                (As: Assem.ASSEM with type temp = Tm.temp
                                  and type temp = Ig.temp
                                  and type temp = Cl.temp
                                  and type label = Tm.label)
                (Fr: Frame.FRAME with type instr = As.instr
                                  and type temp = Tm.temp)
                (Mf: Mkflowgraph.MKFLOWGRAPH 
                 with type instr = As.instr
                  and type flowgraph = Ig.flowgraph)
       : (REGALLOC with type instr = As.instr
                    and type temp = Tm.temp
                    and type frame = Fr.frame) = struct

  type temp = Tm.temp
  type frame = Fr.frame
  type instr = As.instr

  (* a reference identifier that stores a bias spilling cost for temps
     introduced by spilling; must have a value greater than 0 to avoid
     repeated spilling of same temp in a straightline program *)
  let bias : float ref = ref 0.5

  (* a list of currently spilled temporaries *)
  let newtemps: temp list ref = ref []

  (* set up the bias based on the present graph *)
  let setBias (Ig.IGRAPH {graph = g; spillCost = spCost}) =
    let rec getMaxCost xs maxcost = match xs with
      | [] -> maxcost
      | n::ns ->
         let cost:float = spCost (n,1)
         in if cost > maxcost
            then getMaxCost ns cost
            else getMaxCost ns maxcost
    in bias := (getMaxCost (Graph.nodes g) 0.5)


  (* heuristic for deciding best spill---passed to color. We use the
     degree of the node and loop count as determined by igraph, but
     we also want to avoid spilling temporaries created by spills *)
  let spillCost (Ig.IGRAPH {spillCost=spCost; gtemp=gtemp}) (n,deg) =
    let cost = spCost(n,deg)
    in if List.exists (fun x -> (x = gtemp(n))) (!newtemps)
       then cost +. (!bias)
       else cost

  (* removing vacuous moves from an instruction list at the end of 
     reg alloc *)
  let removeVacuous tempMap il =
    let rec removeVacuous_aux xs il = match xs, il with
      | [], il -> il
      | ((As.MOVE {dst=d;src=s;} as i)::il), il' ->
         let d = tempMap d in
         let s = tempMap s
         in if (d = s) 
            then removeVacuous_aux il il'
            else removeVacuous_aux il (i::il')
      | (i::il), il' -> removeVacuous_aux il (i::il')
    in List.rev (removeVacuous_aux il [])


  (* creating a new temporary; call to Tm.newtemp mediated by adding 
     to newtemps *)
  let getTemp () =
    let t = Tm.new_temp() in
    let _ = newtemps := (t :: (!newtemps))
    in t

  (* check if a temporary has been spilled *)
  let getAccess t (tAndAccList: (temp * Fr.access) list) =
    let rec getAccess_aux = function
      | [] -> None
      | ((t',a)::rtas) ->
         if (t=t') then (Some a) else getAccess_aux rtas
    in getAccess_aux tAndAccList

  (* generate a list of loads for the uses associated with an instruction
     to accommodate spilling *)
  let genLoads uses (tAndAccList: (temp * Fr.access) list) =
    let rec genLoads_aux = function
      | [] -> ([], [])
      | (u::uses) ->
         let (newil,newuses) = genLoads_aux uses 
         in (fun o -> match o with
                      | None -> (newil, (u::newuses))
                      | (Some a) -> 
                         let newt = getTemp()
                         in (((Fr.loadSpill(newt,a))::newil),
                             newt::newuses)
            ) (getAccess u tAndAccList)

    in genLoads_aux uses


  (* generate a list of stores for the defs associated with an instruction
     to accommodate spilling *)
  let genStores defs (tAndAccList: (temp * Fr.access) list) =
    let rec genStores_aux = function
      | [] -> ([], [])
      | (d::defs) ->
         let (newil, newdefs) = genStores_aux defs
         in (function None -> (newil,(d::newdefs))
                    | (Some a) ->
                       let newt = getTemp()
                       in (((Fr.storeSpill(newt,a))::newil),
                           newt::newdefs)
            ) (getAccess d tAndAccList)
    in genStores_aux defs

  (* rewriting an instruction (into an sequence of instrs) on spilling *)
  let rewriteInstr i tAndAccList = match i with
    | (As.LABEL _) -> [i]
    | (As.OPER{assem=assem;dst=dst;src=src;jump=jump}) ->
       let (newusesil, newuses) = genLoads src tAndAccList in
       let (newdefsil, newdefs) = genStores dst tAndAccList
       in (if ((not (newuses = [])) || (not (newdefs = [])))
           then newusesil @ 
                  [As.OPER {assem=assem;dst=newdefs;src=newuses;jump=jump}]
                  @ newdefsil
           else [i])
    | As.MOVE{assem=string;dst=d;src=s} ->
          let sacc = getAccess s tAndAccList in
          let dacc = getAccess d tAndAccList
          in (function
                None -> 
                ((function None -> [i]
                         | Some d -> [Fr.storeSpill(s,d)]) dacc)

              | Some s -> 
                 ((function
                     None -> [Fr.loadSpill(d,s)]
                   | Some d -> let t = getTemp()
                               in [Fr.loadSpill(t,s);
                                   Fr.storeSpill(t,d)]
                  )
                    dacc)) sacc

    (* rewriting a program to account for spilling; frame is used to generate
       stack locations for the spills *)
  let rewriteProgram il tl frame =
    let tAndAccList = Fr.spillTemps frame tl in
    let rec rewriteProgram_aux = function
      | [] -> []
      | i::il -> (rewriteInstr i tAndAccList) @
                   (rewriteProgram_aux il)
    in rewriteProgram_aux il


  (* the main procedure for register allocation *)
  let regalloc il regs frame = 
    let rec assignRegs il =
      let ig = Ig.mkIGraph (Mf.instrs2graph il) in
      let _ = setBias ig in
      let (tempMap,spills) = Cl.color {Cl.interference=ig;
                                       spillCost=(spillCost ig);
                                       registers=regs}
      in if (spills = []) 
         then
           ( (Fr.entryExit frame (removeVacuous tempMap il),
              tempMap)
           )
         else
           ( print_endline "Spill list is non-empty. Spilling ";
             (let rec showNames = function
                | [] -> Printf.printf "\n"
                | (t::tl) -> (Printf.printf "%s" ("t" ^ (Tm.temp_name t) 
                                                 ^ " "); 
                                          showNames tl)
              in showNames spills) ;

             assignRegs (rewriteProgram il spills frame)
           )

    in assignRegs il

end
