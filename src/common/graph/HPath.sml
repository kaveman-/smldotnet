(* HPath:HPATH finds an approximate solution to the following NP-complete problem:
   given a complete digraph where the arcs are weighted by non-negative reals, find a sequence of
   vertices beginning with some nominated vertex which has minimum total weight. *)
structure HPath:>HPATH=
struct
   local
      open Graph
      structure OrdNodeMap=SplayMapFn(OrdNode)
      open OrdNodeMap
   in
      fun prefind(Graph.pre{nodes=nodes,arcs=arcs},beginat)=
      let
         val sorted_arcs=WSArcs.wsarcs arcs
         val node_list= List.map #node nodes

         datatype after_node=
            NEXT of node
         |  BEGIN of node

         datatype end_data=END of node
         (* The algorithm we use is very simple.  For each arc in this list, we make its end follow its
            beginning if not already blocked.

            We keep two maps going:

            next, taking nodes to after_node.  If this is NEXT n the node has a designated successor n.
            If it is BEGIN n it hasn't because it's at the end of a block, which begin at n.

            end, taking nodes to end_data.  This map has values for nodes which are at the beginning
            of blocks and also are not the nominated initial block.  If it has a value END n it indicates
            that the end of this block is at n. *)

         val initial_next=
            List.foldl
               (fn (node,map_so_far)=>insert(map_so_far,node,BEGIN node))
               empty
               node_list
         val (initial_begin,_)=
            remove(
               List.foldl
                  (fn (node,map_so_far)=>insert(map_so_far,node,END node))
                  empty
                  node_list
               ,
               beginat
               )

         val (final_next,final_begin)=
            List.foldl
               (fn (({from=from,to=to},_),nb as (next,begin)) =>
                  (case find(begin,to) of
                     NONE => nb
                  |  SOME(END to_end) =>
                     if to_end=from then nb
                     else
                     (case find(next,from) of
                        SOME(NEXT _ ) => nb
                     |  SOME(BEGIN from_begin) =>
                        (* OK - we can do it *)
                        let
                           (* Readjust pointers *)
                           val new_next=
                              insert(insert(next,from,NEXT to),to_end,BEGIN from_begin)
                           val (begin1,_)=
                              remove(begin,to)
                           val new_begin=if from_begin=beginat then begin1 else
                              insert(begin1,from_begin,END to_end)
                        in
                           (new_next,new_begin)
                        end
                     )
                  ))
               (initial_next,initial_begin)
               sorted_arcs
      in
         List.foldr
            (fn (node,list_so_far)=>
            let
               fun block_after(sofar,node)=
                  (case find(final_next,node) of
                     SOME(NEXT n) => block_after(n::sofar,n)
                  |  SOME(BEGIN _) => sofar
                  )
            in
               List.revAppend(block_after([node],node),list_so_far)
            end
            )
         []
         (beginat::(List.map #1 (listItemsi final_begin)))
      end (* find *)
   end (* local *)
end (* HPath *)
