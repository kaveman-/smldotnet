(* WSArcs:WSARCS takes a list of weighted arcs, removes all arcs with the same beginning and end and
   replaces them by an arc with weight their total weight, and sorts the arcs in descending order. *)
signature WSARCS=
sig
   val wsarcs:({from:Graph.node,to:Graph.node}*real) list ->
      ({from:Graph.node,to:Graph.node}*real) list
end
