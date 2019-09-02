open Dynamicarray

module type GRAPH = sig
  type graph
  type node
    
  val nodes: graph -> node list
  val succ: node -> node list
  val pred: node -> node list
  val adj: node -> node list   (* succ+pred *)
  val isadj: node*node -> bool
  val eq: node*node -> bool

  val newGraph: unit -> graph
  val newNode : graph -> node
  exception GraphEdge
  type edge = {from: node; to_: node}
  val mk_edge: edge -> unit
  val rm_edge: edge -> unit

(* ? ? ? 
  Try using the 'a table type in Symbol.

   module Table : TABLE 
    sharing type Table.key = node

 *)
  type 'a table
  val empty : 'a table
  val enter : 'a table -> node -> 'a -> 'a table
  val look  : 'a table -> node -> 'a option



  val nodetoint: node -> int  (* allows arrays to be used sometimes
                                 for node info *)
                                   
  val nodename: node->string  (* for debugging only *)
end

module Graph : GRAPH = struct
(* structure Graph :> GRAPH = *)
  type node' = int

  type noderep = NODE of {succ: node' list; pred: node' list}

  let emptyNode = NODE{ succ=[]; pred=[] }

  let bogusNode = NODE{ succ=[-1]; pred=[] }

  let isBogus = function
    | NODE {succ= -1::_} -> true
    | _ -> false

  type graph = noderep DynamicArray.dynarray
  type node = graph * node'
  type edge = {from: node; to_: node}

  let augment (g: graph) (n: node') : node = (g,n)
    
  let nodes (g: graph) : node list =
    (*let b = DynamicArray.bound g in*)
    let rec f i = if isBogus (DynamicArray.sub g i) then []
              else (g, i) :: f (i + 1)
    in f 0

  let succ ((g, i): node) : node list =
    let NODE {succ=s} = DynamicArray.sub g i
    in List.map (augment g) s

  let pred ((g, i): node) : node list =
    let NODE {pred=p} = DynamicArray.sub g i
    in List.map (augment g) p

  let adj (n: node) : node list = pred n @ succ n

  let isadj (((g, i), (_, j)): node*node) : bool =
    let NODE {pred=p;succ=s} = DynamicArray.sub g i
    in List.exists (fun x -> x = j) (p @ s)

  let eq (((_,a),(_,b)): node*node) : bool = a=b

  let newGraph () = DynamicArray.make 0 bogusNode

  let newNode (g: graph) : node = (* binary search for unused node *)
    let rec look lo hi =
      (* i < lo indicates i in use
       * i >= hi indicates i not in use *)
      if lo=hi then (DynamicArray.update g lo emptyNode; (g, lo))
      else let m = (lo+hi) / 2
           in if isBogus (DynamicArray.sub g m) then look lo m else look (m+1) hi
    in look 0 (1 + DynamicArray.bound g)

  exception GraphEdge
  let check g g' = (* if g=g' then () else raise GraphEdge *) ()

  let rec delete i (ns: node' list) =
    match ns with
    | j::rest -> if i=j then rest else j :: delete i rest
    | [] -> raise GraphEdge

  let diddle_edge change {from=(g, i); to_=(g', j)} =
    let _ = check g g' in
    let NODE {succ=si; pred=pi} = DynamicArray.sub g i in
    let _ = DynamicArray.update g i (NODE {succ=change j si; pred=pi}) in
    let NODE {succ=sj; pred=pj} = DynamicArray.sub g j in
    let _ = DynamicArray.update g j (NODE {succ=sj; pred=change i pj})
    in ()

  let mk_edge ({from: node; to_: node}: edge) = diddle_edge (fun h t -> h :: t) ({from: node; to_: node}: edge)

  let rm_edge ({from: node; to_: node}: edge) = diddle_edge delete ({from: node; to_: node}: edge)

(* ? ? ? 
  Try using the 'a table type in Symbol.

   module Table : TABLE 
    sharing type Table.key = node
 *)
  (* This is a very inefficient implementation of a functional table.
     Can you do better?
   *)
  type 'a table = (node * 'a) list
  let empty = []
  let enter t s v = (s, v) :: t
  let look t s' = 
    match List.find_opt (fun (s, v) -> s = s') t with
    | None -> None
    | Some (s, v) -> Some v


  let nodetoint ((g, i): node) : int = i

  let nodename ((g, i): node) : string = "n" ^ (string_of_int i)
end

