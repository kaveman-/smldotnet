(*======================================================================*)
(* Dependency analysis.							*)
(* See signature for more details.                                      *)
(*======================================================================*)
structure Dep :> DEP =
struct

(*----------------------------------------------------------------------*)
(* Topological sorting for dependency analysis				*)
(* Adapted from Section 3.17 of "ML for the Working Programmer", 	*)
(* L. C. Paulson, 2nd edition, 1996, CUP.				*)
(*----------------------------------------------------------------------*)
fun cyclesort (graph, eq) =
let
  fun newvisit (x, (visited,cys)) = (x::visited, cys)

  fun mem (n, []) = false
    | mem (n, n'::path) = eq(n,n') orelse mem (n, path) 

  fun adj n =
  let fun f [] = [] (* raise Fail "Dep.cyclesort: missing node" *)
        | f ((n',adj)::graph) = if eq(n',n) then adj else f graph
  in f graph end

  fun sort ([], path, (visited,cys)) = (visited, cys)
    | sort (n::ns, path, (visited,cys)) =
      sort (ns, path, 
         if mem(n, path) then (visited, n::cys)
         else 
         if mem(n, visited) then (visited, cys)
         else newvisit(n, sort(adj n, n::path, (visited,cys))))
in 
  sort(map #1 graph, [], ([],[])) 
end

(*----------------------------------------------------------------------*)
(* Strongly-connected components wrapper.				*)
(* George's interface is rather unwieldy; here we provide a less	*)
(* general interface to it.						*)
(*----------------------------------------------------------------------*)
datatype 'a Component =
  NonRec of 'a
| Rec of 'a list

fun scc (graph, eq) =
let 
  val pairs = map (fn (n,adj) => ((n,adj), Graph.newNode())) graph

  fun findNode n = 
  let fun find [] = raise Fail "SCC.scc: lost node"
        | find (((n',adj), graphnode)::rest) = 
          if eq(n',n) then graphnode else find rest
  in 
    find pairs
  end
    
  fun isRecursive (n,[]) = false
    | isRecursive (n,n'::rest) = eq(n,n') orelse isRecursive (n,rest)

  val arcs = List.concat 
    (map (fn ((n, adj), node) => map (fn n' => (node, findNode n')) adj) pairs)

  val g = Graph.makeGraph (Graph.pre 
    { nodes = map (fn ((n,adj),node) => {node = node, label = (n,adj)}) pairs,
      arcs = map (fn (from,to) => ({from = from, to = to}, ())) arcs
    })

  val sccs = StrongComponents.strong_components g

  fun makeSCC scc =
  case scc of
    [] => 
    raise Fail "Dep.scc.makeSCC: empty component"

  | [graphnode] =>
    let 
      val (n,adj) = valOf (Graph.nodeLabel(g, graphnode))
    in
      if isRecursive (n,adj) then Rec [n]
      else NonRec n
    end

  | _ =>
    let
      fun convert graphnode = #1 (valOf (Graph.nodeLabel(g, graphnode)))
    in
      Rec (map convert scc)
    end
in
  map makeSCC sccs
end

end
