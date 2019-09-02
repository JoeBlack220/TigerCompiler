(* Copyright (C) 2007 Gopalan Nadathur 

   Translated to OCaml by Eric Van Wyk.
 *) 

open Graph

module type MKFLOWGRAPH = sig
  type instr
  type flowgraph

  (* the assumption here is that the nodes list that is returned
     has the entry point as the first node and the exit as the last *)
  val instrs2graph : instr list -> flowgraph * Graph.node list
end

module MkFlowGraph
                   (Sy : Symbol.SYMBOL) 
                   (FG : Flowgraph.FLOW)
                   (As: Assem.ASSEM with type label = Sy.symbol
                                     and type temp = FG.temp)
                            : (MKFLOWGRAPH with type instr = As.instr
                                            and type flowgraph = FG.flowgraph) =
struct
   type instr = As.instr
   type flowgraph = FG.flowgraph


(* flowgraph is created in the following steps:

      1. nodes are added for each instruction and a table of nodes indexed
         by labels is created; the use, def and ismove maps are also 
         created at this time

      2. edges are added to the graph; the edge uses the jump labels if
         any else it adds an edge following straight line flow

      3. graph is traversed using a dfs to find loops and the loopcount 
         mapping is created
*)
let instrs2graph il =
      let g = Graph.newGraph() in

          (* function for adding nodes for instructions *)
          let addNode (def,use,ismove,ltable,nodes) (i:instr) =
            match i with
            | (As.OPER {dst = d; src = s; jump = j}) ->
                let node = (Graph.newNode g)
                in (Graph.enter def node d, 
                    Graph.enter use node s,
                    Graph.enter ismove node false,
                    ltable,node::nodes)
            | (As.LABEL {lab = lab}) ->
                let node = (Graph.newNode g)
                in (Graph.enter def node [],
                    Graph.enter use node [],
                    Graph.enter ismove node false,
                    Sy.enter ltable lab node,
                    node::nodes)
            | (As.MOVE {dst = d; src = s}) ->
                let node = (Graph.newNode g)
                in (Graph.enter def node [d], 
                    Graph.enter use node [s],
                    Graph.enter ismove node true,
                    ltable, node :: nodes)
          in

          (* actually make new nodes for each instruction; also create 
             an indexing of nodes by labels for introducing edges later *)
          let (def,use,ismove,ltable,nodes) =
                 (List.fold_left addNode 
                        (Graph.empty,Graph.empty,
                         Graph.empty,Sy.empty,[])
                        il) in

          let nodes = (List.rev nodes) in

          (* add an edge to the next node in sequence if there is one *)
          let chainEdge ns n =
            match ns with
            | [] -> ()
            | (n'::ns') -> Graph.mk_edge {from = n; Graph.to_ = n'}
          in

          let app f xs = List.fold_left (fun _ x -> f x) () xs in

          (* add edges between nodes based on instruction form *)
          let rec addEdges instrs ns =
            match instrs with
            | [] -> ()
            | ((As.OPER {jump = (Some js)})::il) ->
                let succs = 
                         (List.map 
                           (fun j ->
                              (fun m -> match m with
                                         | Some s -> s
                                         | None -> 
                                             raise 
                                               (Errormsg.ErrorMsg.impossible 
                                                       "Undefined jump found\
                                                          \ in flowgraph construction"))
                              (Sy.look ltable j))
                           js)
                in (app (fun s -> (Graph.mk_edge {from=(List.hd ns); Graph.to_=s})) succs);
                   addEdges il (List.tl ns)
            | (_::il) ->
               (chainEdge (List.tl ns) (List.hd ns) ; addEdges il (List.tl ns))
          in

          let _ = addEdges il nodes in

          let lc = (List.fold_left (fun tab n -> Graph.enter tab n 1) 
                          Graph.empty
                          nodes)
          in

          let marked = Array.make (List.length nodes) false in

          (* loops are found by conducting a dfs of the graph and 
             remembering the nodes in the current path being developed.
             When a loop is found, loop count is incremented by one for 
             all nodes in the loop. We only look for loops that are reachable
             from the start node.
          *)
          let rec setLoopCounts node =
               let nodeindex = Graph.nodetoint node in

                   let rec updateLoopNodes n start =
                         let rec findNext nns = match nns with
                           | n::ns ->
                              if (Array.get marked (Graph.nodetoint n))
                              then n
                              else findNext ns
                           | _ -> failwith "error in updateLoopNodes"
                         in 
                            let valOf (o: 'a option) =
                              match o with
                              | Some v -> v
                              | None   -> raise (Invalid_argument "valOf None")
                            in
                            (* FIXME: is this supposed to have the side-effect
                             * of updating lc? *)
                            ignore (Graph.enter lc
                                              n
                                              ((valOf (Graph.look lc n))
                                                       + 1))
                                             ;
                            if Graph.eq(n, start)
                            then ()
                            else (updateLoopNodes (findNext (Graph.succ n))
                                                  start)
                   in


                   let checkLoops succ =
                        if (Array.get marked (Graph.nodetoint succ))
                        then updateLoopNodes succ node
                        else setLoopCounts succ

               in Array.set marked nodeindex true;
                  app checkLoops (Graph.succ node); 
                  Array.set marked nodeindex false
          in

          let _ = (fun xs -> (match xs with
                             | [] -> ()
                             | (h::_) -> setLoopCounts h)) nodes

      in (FG.FGRAPH {control = g; def = def; use = use;
                     loopcount = lc; ismove = ismove},
          nodes)
end
