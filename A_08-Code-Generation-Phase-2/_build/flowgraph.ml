open Graph

module type FLOW = sig
  type temp 

  type flowgraph = FGRAPH of {control: Graph.graph;
			      def: temp list Graph.table;
			      use: temp list Graph.table;
                              loopcount: int Graph.table;
			      ismove: bool Graph.table
                             }

  (* given a list of nodes with has the entry point as the first and 
     the exit as the last, this function produces a reverse topological 
     sort of the nodes *)
  val revTopSort: Graph.node list -> Graph.node list

  val show : out_channel -> flowgraph -> unit
end

module Flow (Tm : Temp.TEMP) : (FLOW with type temp = Tm.temp) = struct

  type temp = Tm.temp

  type flowgraph = FGRAPH of {control: Graph.graph;
			      def: Tm.temp list Graph.table;
			      use: Tm.temp list Graph.table;
                              loopcount: int Graph.table;
			      ismove: bool Graph.table}

  (* Note:  any "use" within the block is assumed to be BEFORE a "def" 
        of the same variable.  If there is a def(x) followed by use(x)
       in the same block, do not mention the use in this data structure,
       mention only the def.

     More generally:
       If there are any nonzero number of defs, mention def(x).
       If there are any nonzero number of uses BEFORE THE FIRST DEF,
           mention use(x).

     For any node in the graph,  
           Graph.Table.look(def,node) = SOME(def-list)
           Graph.Table.look(use,node) = SOME(use-list)
   *)

  let revTopSort (nodes: Graph.node list) : Graph.node list =
    let marked = Array.make (List.length nodes) false
    in
    let rec dfs (n: Graph.node) (nl: Graph.node list): Graph.node list =
      let i = Graph.nodetoint n
      in if not (Array.get marked i)
         then 
           ( (Array.set marked i true);
             n :: (List.fold_left (fun nl n -> dfs n nl) nl (Graph.pred n))
           )
         else nl
    in List.fold_right dfs nodes []

  let valOf (o: 'a option) =
    match o with
    | Some v -> v
    | None   -> raise (Invalid_argument "valOf None")

  let show out (FGRAPH {control=c; def=d; use=u; ismove=move}) = 
       let rec showtemplist = function
         | [] -> Printf.fprintf out " "
         | (t::ts) ->
            ( Printf.fprintf out "%s" (Tm.make_string t);
              Printf.fprintf out " ";
              showtemplist ts
            )
       in
       let rec shownodeslist = function
         | [] -> ()
         | n::ns ->
            ( Printf.fprintf out "%s" (Graph.nodename n);
              Printf.fprintf out " ";
              shownodeslist ns
            )
       in
       let showonenode n =
         ( Printf.fprintf out "%s" (Graph.nodename n);
           Printf.fprintf out ": ";

           (let dl = valOf (Graph.look d n)
            in showtemplist dl
           );

           (let m' = valOf (Graph.look move n)
            in if m'
               then Printf.fprintf out "<= "
               else Printf.fprintf out "<- "
           );

           (let ul = valOf (Graph.look u n)
            in showtemplist ul
           );

           Printf.fprintf out "; goto ";
           shownodeslist (Graph.succ n);

           Printf.fprintf out "\n"
         )
       in List.iter showonenode (Graph.nodes c)

end
