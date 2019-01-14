(* Partition:PARTITION does not use the Graph structure but is in this
   directory because it can be used for many graph algorithms.  It
   is a functor which takes an ORD_MAP indexing things of type elem to
   a structure which manipulates partitions on things of type elem *)
functor Partition(A:ORD_MAP):PARTITION =
struct
   type elem=A.Key.ord_key
   type part=elem
(* The elem is some member of the part. *)
   structure OrdPart=A.Key

   type partition=part A.map * (int*elem list) A.map
(* The part A.map gives the map from elements to their corresponding
   parts. the int* elem list A.map gives the map from parts to
   the tuple (number of elements in the part,list of elements in the
   part). *)
   fun new_partition l=
   let
      fun add_one1(el,map_so_far)=A.insert(map_so_far,el,el)
      val partition1=List.foldl add_one1 A.empty l
      fun add_one2(el,map_so_far)=A.insert(map_so_far,el,(1,[el]))
      val partition2=List.foldl add_one2 A.empty l
   in
      (partition1,partition2)
   end

   fun find_part((partition1,_),el)=valOf(A.find(partition1,el))

   fun union(p:partition,el1:part,el2:part)=
      if A.Key.compare(el1,el2)=EQUAL
      then
      (* the two parts are equal *)
         p
      else
      let
         val (p1,p2)=p
         val SOME (n1,l1)=A.find(p2,el1)
         val SOME (n2,l2)=A.find(p2,el2)
         (* A Match exception here indicates that the element was not in
            the original list supplied with the partition. *)
         fun make_new_p1(new_el,old_list)=
(* return p2 where the members of old_list are changed to point to new_el *)
            List.foldl
               (fn (el_toc,map_so_far)=>A.insert(map_so_far,el_toc,new_el))
               p1
               old_list
      in
         if n1<n2
         then
            (* move everything to the part represented by el2 *)
            let
               val (new_p2_1,_)=A.remove(p2,el1)
               val new_p1=make_new_p1(el2,l1)
               val new_p2=A.insert(new_p2_1,el2,(n1+n2,List.revAppend(l1,l2)))
            in
               (new_p1,new_p2)
            end
         else
            (* move everything to the part represented by el1 *)
            let
               val (new_p2_1,_)=A.remove(p2,el2)
               val new_p1=make_new_p1(el1,l2)
               val new_p2=A.insert(new_p2_1,el1,(n2+n1,List.revAppend(l1,l2)))
            in
               (new_p1,new_p2)
            end
      end

   fun union_list(p:partition,[])=p
   |   union_list(p:partition as (p1,p2),els)=
   let
      val part_lists=
         List.map
            (fn el =>
               let
                  val SOME part = A.find(p1,el)
                  val SOME nl = A.find(p2,part)
               in
                  (part,nl)
               end 
             )
            els

      val maximum as (pmax,(nmax,lmax))=
         List.foldl
            (fn (pl1 as (_,(n1,_)),pl2 as (_,(n2,_))) =>
               if n1>n2 then pl1 else pl2
               )
            (hd part_lists)
            (tl part_lists)

      val (p2,_) = A.remove(p2,pmax) 
      (* We use p2 to keep track of what has already been copied.
         This means that duplicate elements are ignored *) 
     
      val (p1,p2,nnew,lnew)=
         List.foldl
            (fn(part as (p,(np,lp)),orig as (p1,p2,nmax,lmax)) =>
               let
                  val (p2,_)=A.remove(p2,p)
                  val p1 =
                     List.foldl
                        (fn (el,p1) => A.insert(p1,el,pmax))
                        p1
                        lp
               in
                  (p1,p2,nmax+np,List.revAppend(lp,lmax))
               end handle LibBase.NotFound => orig
               )
            (p1,p2,nmax,lmax)
            part_lists

      val p2=A.insert(p2,pmax,(nnew,lnew))
   in
      (p1,p2)
   end   

   fun list_part((_,p2),el)=
   let
      val SOME (_,l)=A.find(p2,el)
   in
      l
   end

   fun list_parts(p1,p2)=List.map (fn (el,_)=>el) (A.listItemsi p2)
end
