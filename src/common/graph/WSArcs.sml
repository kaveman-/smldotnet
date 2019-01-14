(* WSArcs:WSARCS takes a list of weighted arcs, removes all arcs with the same beginning and end and
   replaces them by an arc with weight their total weight, and sorts the arcs in descending order. *)
structure WSArcs:>WSARCS=
struct
   local
      open Graph
      structure OrdArcNode:>ORD_KEY 
         where type ord_key=({from:node,to:node}*real)=
      struct (* this is used for sorting the arcs.  Fortunately our
                arcs are monomorphic *)
         type ord_key=({from:node,to:node}*real)
         fun compare((_,r1),(_,r2))=Real.compare(r1,r2)
      end
      structure OrdArcSort=Sort(OrdArcNode)

      structure NodePairKey:ORD_KEY=
      struct (* this is used for removing duplicate arcs. *)
         type ord_key={from:node,to:node}
         fun compare({from=f1,to=t1},{from=f2,to=t2})=
            (case OrdNode.compare(f1,f2) of
                LESS=>LESS
            |   GREATER=>GREATER
            |   EQUAL=>OrdNode.compare(t1,t2))
      end
      structure NodePairMap=SplayMapFn(NodePairKey)
   in
      fun wsarcs arc_list=
      let
         val arc_map= (* construct a map from node pairs to reals *)
            List.foldl
            (fn ((nodepair,weight),map_so_far)=>
                (case NodePairMap.find(map_so_far,nodepair) of
                   SOME oldweight=>
                      NodePairMap.insert(map_so_far,nodepair,
                         oldweight+weight)
                |  NONE=>
                      NodePairMap.insert(map_so_far,nodepair,weight)
                ))
            NodePairMap.empty
            arc_list

         val arcs2=NodePairMap.listItemsi arc_map
      in
         (* Sort the arcs in descending order of weight. *)
         List.rev(OrdArcSort.sort arcs2)
      end
   end
end
