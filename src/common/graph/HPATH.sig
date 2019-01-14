(* HPath:HPATH finds an approximate solution to the following NP-complete problem:
   given a complete digraph where the arcs are weighted by non-negative reals, find a sequence of
   vertices beginning with some nominated vertex which has minimum total weight. *)
signature HPATH=
sig
   val prefind:('a,real) Graph.preGraph*Graph.node -> Graph.node list
   (* the node supplied is the nominated one.  Like OrthogonalPartition, it is actually more convenient
      to work with the preGraph than the graph.

      If multiple arcs are supplied with the same beginning and end, they are replaced by a single
      arc with weight their total weight.
      *)
end
