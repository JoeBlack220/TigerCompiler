(* Copyright (C) 2007 Gopalan Nadathur 

   Translated to OCaml by Eric Van Wyk
*) 

open Graph

module type IGRAPH = sig 
  type temp
  type flowgraph

  type igraph =
    IGRAPH of { graph: Graph.graph;
                tnode: temp -> Graph.node option;
                gtemp: Graph.node -> temp;
                moves: (Graph.node * Graph.node) list;
                spillCost: Graph.node * int -> float
              }

  val mkIGraph : flowgraph * Graph.node list -> igraph

  val show : out_channel -> igraph -> unit
end

module IGraph (Tm: Temp.TEMP)
              (Fg: Flowgraph.FLOW with type temp = Tm.temp)
       : (IGRAPH with type temp = Tm.temp
                  and type temp = Fg.temp
                  and type flowgraph = Fg.flowgraph) = struct


  type temp = Tm.temp

  type flowgraph = Fg.flowgraph

  type igraph =
    IGRAPH of { graph: Graph.graph;
                tnode: temp -> Graph.node option;
                gtemp: Graph.node -> temp;
                moves: (Graph.node * Graph.node) list;
                spillCost: Graph.node * int -> float
              }

  type nodesets_rec = {into: bool array; out: bool array; def: bool array}

  (* cost of being in a loop for a temporary as a discouragement 
     for spilling *)
  let loopfactor = 10

  let valOf (o: 'a option) =
    match o with
    | Some v -> v
    | None   -> raise (Invalid_argument "valOf None")

  let mkIGraph (Fg.FGRAPH {control=c; def=d; use=u; loopcount=lc; 
                           ismove=m}, nl) =
    let numtemps = (ref 0)
    in
    (* a function for creatting a bitmap of all the temporaries used in a
     lis t of flowgraph nodes and for collecting these into a list *)
    let mkBitMap nl =
      let rec addTemps tmps map_and_ts = match tmps, map_and_ts with
        | [], _ -> map_and_ts
        | (t::ts), (bm,allts) ->
           addTemps ts 
                    ((function (Some _) -> map_and_ts
                             | None -> let n = !numtemps 
                                       in numtemps := n + 1;
                                          (Tm.enter bm t n),
                                          (t::allts)
                     )
                     (Tm.look bm t))

        and mkBitMap_aux ns map_and_ts = match ns with
          | [] -> map_and_ts
          | n::ns ->
             mkBitMap_aux ns 
                          (addTemps (valOf(Graph.look d n))
                                    (addTemps 
                                        (valOf(Graph.look u n))
                                        map_and_ts))

      in mkBitMap_aux nl (Tm.empty, [])
    in

    (* actually create the bitmap and collect the temporaries now *)
    let (bitmap,allts) = mkBitMap nl
    in
    (* rep for a reverse mapping from bit number to temporaries *)
    let rbitmap = Array.make (!numtemps) (Tm.new_temp ())
    in

    (* setting up the reverse mapping *)
    let _ = let rec mkRBitMap ts n = match ts with 
              | [] -> ()
              | t::ts ->
                 ((Array.set rbitmap n t); mkRBitMap ts (n-1))
            in mkRBitMap allts (!numtemps - 1)
    in

    let initNodeSets ns =
      let rec initOneBAFromTemps ba ts = match ts with 
        | [] ->  ()
        | t::ts -> ( (Array.set ba (valOf(Tm.look bitmap t)) true);
                     (initOneBAFromTemps ba ts)
                   )
      in
      let rec initAllFromTemps ns nodesets = match ns with
        | [] -> nodesets
        | n::ns ->
           let into = Array.make (!numtemps) false in
           let out = Array.make (!numtemps) false in
           let def = Array.make (!numtemps) false
           in initOneBAFromTemps into (valOf (Graph.look u n));
              initOneBAFromTemps def (valOf (Graph.look d n));
              initAllFromTemps ns (Graph.enter nodesets n
                                         {into=into;out=out;def=def})
      in initAllFromTemps ns Graph.empty
    in

    let nodesets = initNodeSets nl
    in


    (* finding the fixed point for the in and out sets of assem nodes 
       based on liveness equations *)
    let findLiveSets ns =
      let sortedns = Fg.revTopSort (List.rev ns)
      in
      let changed = ref false
      in
      (* calculate the OR of ith IN bit of successors *)
      let rec orOfSuccsInBit ns i = match ns with 
        | [] -> false
        | n::ns ->
           (let {into=into} = valOf (Graph.look nodesets n)
            in if (Array.get into i) 
               then true
               else orOfSuccsInBit ns i)

      in
      (* update the ith bit of s based on condition c *)
      let rec oneSet c s i = 
        if (i >= !numtemps) then ()
        else 
          ( (if (c i) 
             then (Array.set s i true; changed := true)
             else ());
            oneSet c s (i+1)
          )
      in                 
      (* update live in and out sets based on one iteration *)
      let rec oneIter ns = match ns with
        | [] ->  ()
        | n::ns ->
           let {def=def;into=into;out=out} =
             valOf (Graph.look nodesets n)
           in oneSet (fun j ->  (not (Array.get out j)) &&
                                  (orOfSuccsInBit (Graph.succ n) j))
                     out 0;
              oneSet (fun j -> (not (Array.get into j)) &&
                                 (not (Array.get  def j)) &&
                                   (Array.get out j))
                     into 0;
              oneIter ns
      in
      (* iterate to a fixed point *)
      let rec iter () = 
        ( oneIter sortedns;
          if (!changed) 
          then (changed := false; iter ())
          else ()
        )
      in iter ()

    in
    let _ = findLiveSets nl
    in

    (* identifier representing the igraph (to be) created *)
    let igraph = Graph.newGraph()
    in

    (* a function for populating the igraph with nodes generated from 
       a list of temporaries; also sets up back and forth mappings *)
    let rec mkIGraphNodes ts maps = match ts, maps with
      | [], _ ->  maps
      | (t::ts), (toTemp,toNode) ->
         let node = Graph.newNode igraph
         in  mkIGraphNodes ts (Graph.enter toTemp node t,
                               Tm.enter toNode t node)
    
    in       
    (* a table that eventually maps temporaries to igraph nodes *)         
    (* a table mapping from igraph nodes to temporaries *)
    let (toTemp,toNode) = mkIGraphNodes allts 
                                        (Graph.empty, Tm.empty)
    in
    (* interface to the to and fro mappings between nodes and temps *)
    let tnode t = Tm.look toNode t
    in
    let gtemp n = valOf (Graph.look toTemp n)
    in
    let mytnode t = valOf (Tm.look toNode t)
    in
    (* add arcs for move instructions; for moves, there is an 
       arc from what is defined to what is live out only if the 
       live out is not the source and not what is defined *)
    let addInterferenceMove def use out =
      let  dindex = valOf (Tm.look bitmap def) in
      let uindex = valOf (Tm.look bitmap use) in
      let dnode = mytnode def in
      let rec iter n =
        if (n < !numtemps)
        then ( if (Array.get out n &&
                     (n <> dindex) && (n <> uindex))
               then let to_ = mytnode (Array.get rbitmap n)
                    in (if (not (Graph.isadj(dnode,to_)))
                        then Graph.mk_edge {from=dnode; Graph.to_=to_}
                        else ())
               else ();
               iter (n+1)
             )
        else ()
      in iter 0

    in

    (* add arcs for non-moves; here all defs are assumed to be 
       after all uses. *)
    let addInterferenceNonMove dl out =
      let rec addEdgestoOne to_ fs = match fs with
        | [] -> ()
        | f::fl ->
           ( let from = mytnode f
             in (if ((not (Graph.eq (to_,from)))
                    && (not (Graph.isadj(from,to_)))) 
                then Graph.mk_edge{from = from; Graph.to_ = to_}
                else ()
                );
                addEdgestoOne to_ fl
           )
      in
      let rec addEdgestoAll i = 
        if (i < !numtemps) 
        then (if (Array.get out i) 
              then addEdgestoOne 
                     (mytnode (Array.get rbitmap i)) dl
              else ();
              addEdgestoAll (i+1))
        else ()
      in addEdgestoAll 0


    in
    (* add edges in interference graph based on liveness sets associated 
       with flowgraph nodes; in the end return a list of move related 
       temp pairs *)
    let rec mkIGraphEdges ns moves = match ns with
        | [] -> moves
        | n::ns ->
           let src = valOf (Graph.look u n) in
           let dst = valOf (Graph.look d n) in
           let {out=out} = valOf (Graph.look nodesets n)
           in 
           if (valOf (Graph.look m n))
           then let s = match src with [s] -> s | _ -> failwith "impossible" 
                in
                let d = match dst with [d] -> d | _ -> failwith "impossible" 
                in addInterferenceMove d s out; 
                   mkIGraphEdges ns ((mytnode d,mytnode s)::moves)
           else ( addInterferenceNonMove dst out; 
                  mkIGraphEdges ns moves
                )
    in

    let moves = mkIGraphEdges nl [] 
    in

    (* array storing a spilling cost for each temp node *)
    let spillcost = Array.make (!numtemps) 0.0
    in

    (* setting up the heuristic value for each temp node; spilling cost 
       should be high if the node is in many loops *)
    let rec updateSpillCosts ns =
      let rec updateSpillCostsOneSet ts lc = match ts with
        | [] -> ()
        | t::ts ->
           ( let ni = Graph.nodetoint (mytnode t)
             in (Array.set spillcost ni 
                          ((Array.get spillcost ni)  +. 
                             (float_of_int (loopfactor * lc)))
                );
                updateSpillCostsOneSet ts lc
           )
      and updateSpillCosts_aux ns = match ns with
        | [] -> ()
        | n::ns ->
           ( ( let lc = valOf (Graph.look lc n)
               in 
               ( (updateSpillCostsOneSet (valOf (Graph.look u n)) lc);
                 (updateSpillCostsOneSet (valOf (Graph.look d n)) lc)
               )
             );
             updateSpillCosts_aux ns
           )
      in updateSpillCosts_aux ns
    in
    let _ = updateSpillCosts nl
    in

    (* a function for calculating the actual heuristic; cost should be 
       mediated by benefit measured in terms of the number 
       of interferences that will be affected *)
    let spillCost (n,deg) = (Array.get spillcost (Graph.nodetoint n)) /. 
                              (float_of_int deg)
                            
    in 
    IGRAPH {graph = igraph; tnode = tnode;
            gtemp = gtemp; moves = moves; spillCost = spillCost}

    
      
  let show out (IGRAPH{graph = g; gtemp = gtemp}) =
    let rec showadj ns = match ns with
      | [] -> Printf.fprintf out "\n"
      | n::ns ->
         ( Printf.fprintf out "%s" (Tm.make_string (gtemp n));
           Printf.fprintf out " ";
           showadj ns
         )
    and shownodes ns = match ns with
      | [] -> Printf.fprintf out "\n"
      | n::ns ->
         ( Printf.fprintf out "%s" (Tm.make_string (gtemp n));
           Printf.fprintf out ": ";
           showadj (Graph.adj n);
           shownodes ns
         )
    in shownodes (Graph.nodes g)

end
