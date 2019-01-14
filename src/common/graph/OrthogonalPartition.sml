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
functor OrthogonalPartition(A:PARTIALINTSET):>ORTHOGONALPARTITION
   where type intitem=A.intitem where type intset=A.intset =
struct
   local
      open Graph
      structure NodePart=Partition(SplayMapFn(OrdNode))
      (* NodePart implements partitions on nodes *)
      structure PartMap=SplayMapFn(NodePart.OrdPart)
      (* We use PartMap for getting from parts to their corresponding
         intset *)
   in
      type intitem=A.intitem
      type intset=A.intset
      fun prefind(pre {nodes=node_list,arcs=arc_list}:('a,real) preGraph,
         intitemfun:'a->intitem)=
      let
         (* Replace the arcs by equivalent ones in which from<=to. *)
         val arcs1=
            List.map
               (fn arc as ({from=f,to=t},c)=>
                  if OrdNode.compare(f,t)=GREATER
                  then ({from=t,to=f},c)
                  else arc
                  )
               arc_list

         (* remove duplicate arcs and sort in descending order of weight *)
         val sorted_arcs=WSArcs.wsarcs arcs1

         (* find the nodes of the graph *)
         val nodes=List.map #node node_list

         open NodePart
         val first_Q=new_partition nodes
         (* Construct a map taking each part to the corresponding intset *)
         val first_is_map=
            List.foldl
               (fn({node,label},map_so_far)=>
                  PartMap.insert(map_so_far,find_part(first_Q,node),
                     A.singleton(intitemfun label)))
               PartMap.empty
               node_list

         val (final_Q,final_is_map)=
            List.foldl
               (fn(({from=u,to=v},_),nogo as (Q_sofar,is_map_sofar))=>
(* if the parts containing u and v differ, replace
   them by their union, provided the two associated intsets differ. *)
               let
                  val partu=find_part(Q_sofar,u)
                  val partv=find_part(Q_sofar,v)
               in
                  if OrdPart.compare(partu,partv)=EQUAL
                  then nogo
                  else
                  let
                     val u_intset=valOf(PartMap.find(is_map_sofar,partu))
                     val v_intset=valOf(PartMap.find(is_map_sofar,partv))
                  in
                     if A.intersects(u_intset,v_intset)
                     then nogo
                     else
                     let
                        val (is_map_u,_)=PartMap.remove(is_map_sofar,partu)
                        val (is_map_u_v,_)=PartMap.remove(is_map_u,partv);
                        val uv_intset=A.union(u_intset,v_intset)
                        val new_Q=union(Q_sofar,partu,partv)
                        val partuv=find_part(new_Q,u)
                        val new_is_map=PartMap.insert(is_map_u_v,partuv,uv_intset)
                     in
                        (new_Q,new_is_map)
                     end
                  end
               end
               )
               (first_Q,first_is_map)
               sorted_arcs
      in
         List.map
            (fn (part,intset) => (list_part(final_Q,part),intset))
            (PartMap.listItemsi final_is_map)
      end (* prefind *)
   end (* local *)
end (* struct *)
