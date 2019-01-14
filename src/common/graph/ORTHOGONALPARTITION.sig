(* OrthogonalPartition is a functor which takes a PARTIALINTSET structure
   to ORTHOGONALPARTITION.  It tries to solve the following problem:

   Given an undirected graph (V,E) with non-negative edge weights
   given by w:E->R, where each node is associated with an intitem
   find a partition Q where for each set in Q the intset associated with that
   set was constructed with no intersections; this partition is to maximise
   the total weight of arcs which stay in the same set.
   This functor is used for
   local assignment.  Roughly speaking, the nodes correspond to values in
   the blocks, and we try to ensure that the argument value to a block is
   in the same local as the computed value passed to it.  The intersections
   correspond to when two values are live and stored in the same basic
   block at the same time, and so must go to different local variables.

   The problem is NP-complete, by a fairly economical reduction from
   Maximum Independent Set.  In fact, if every set in P has size 1 except for
   one of size 3, it is still NP-complete, by a less economical reduction
   from 3-colouring, so we can't hope for much.

   The approximate algorithm
   used is this: take the edges in decreasing order of weight, and for
   each edge in this order, join the two ends if the associated intsets
   do not intersect.  A PARTITION structure is
   used to keep track of the partition as we go.  It should be possible
   to improve the performance of this algorithm by contracting the
   arcs and always taking the heaviest remaining feasible one.
   *)
signature ORTHOGONALPARTITION=
sig
   type intitem
   type intset (* these comes from the functor *)
   val prefind:('a,real) Graph.preGraph*('a->intitem) -> (Graph.node list * intset) list
   (* Returned is a list containing, for each set in the partition, the list of nodes in it, and
      the intset which corresponds to its union.

      It is actually more convenient to
      use a preGraph rather than a graph for this function.  The edges
      should only be given once (not in both directions).  Where there
      are multiple edges between two nodes, we replace them by
      a single edge with weight the total of the weights of the
      replaced edges. *)
end
