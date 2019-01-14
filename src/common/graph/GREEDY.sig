(* Greedy:GREEDY greedy-colours a graph given a list of its nodes.  Thus,
   if N_i is the i^th node in the list, N_i gets coloured with the least
   positive integer which isn't colouring any node in {N_j | j<i &
   N_j is adjacent to N_i}.  At the moment, no checks are made to verify
   that either (1) the graph has no loops or (2) the graph is symmetric
   (if A is adjacent to B then B is adjacent to A) or (3) the node list
   is a list of the nodes of the graph, without repetitions; the effect of
   breaking any of these rules is undefined.
   *)
signature GREEDY=
sig
   val greedy:(('n,'a) Graph.graph*(Graph.node list))->(Graph.node*int) list
   (* the colouring is returned as a list of pairs (node,its colour) *)
   val mindeg_greedy:(('n,'a) Graph.graph)->(Graph.node*int) list
   (* this colours using the following algorithm: take a copy of the graph.
      Repeatedly remove some node of minimum degree from this graph until
      there is nothing left.  Reverse the order of nodes so obtained, and
      then pass this to greedy. *)
end
