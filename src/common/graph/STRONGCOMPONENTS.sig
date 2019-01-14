(* StrongComponents:STRONGCOMPONENTS finds the strongly connected components
   of a graph.

   The algorithm used is a fairly straight translation of that in
   Fig 1.20 of Gibbons, "Algorithmic Graph Theory" (1985).  It would probably
   not be easy to understand it without reference to the book.  The only
   changes are that we avoid array references and instead use refs attached
   to nodes of the graph.
   *)
signature STRONGCOMPONENTS=
sig
   val strong_components:('n,'a) Graph.graph->Graph.node list list
   (* The output of strong_components is a list of lists of nodes.
      This is a partition of the node set of the graph, where each
      list of nodes contains the nodes in a strongly connected component.
      Furthermore, the list of lists of nodes is topologically sorted,
      in the following sense: if C1 and C2 are two strong connected
      components, and there is a path from C1 to C2, then the list
      for C1 occurs before the list for C2.
      *)
end
