(* Greedy:GREEDY greedy-colours a graph given a list of its nodes.  Thus,
   if N_i is the i^th node in the list, N_i gets coloured with the least
   positive integer which isn't colouring any node in {N_j | j<i &
   N_j is adjacent to N_i}.  At the moment, no checks are made to verify
   that either (1) the graph has no loops or (2) the graph is symmetric
   (if A is adjacent to B then B is adjacent to A).
   *)
structure Greedy:>GREEDY=
struct
   local
      open Graph
   in

      (* first_not_in returns the first positive integer not in the supplied
   	 list of positive integers.  I apologise for the array, but I can't
         think of any other way
   	 of doing this in linear time, modulo garbage collection *)
      fun first_not_in l=let
   	 val list_length=List.length(l)
   	 (* The first positive integer not in the list is certainly in the
   	    set [1,list_length+1].  Therefore, we allocate a boolean array
   	    of length list_length+1 (allowing for the unused element index 0)
   	    and use it to keep track of what integers in the interval are
   	    in the list.  *)
   	 val arr=Array.tabulate (list_length+2, fn _ => false)
   	 val _ =
   	    List.app
   	      (fn el=>
   		  if el<=list_length
   		  then
   		     Array.update(arr,el,true)
   		  else
   		     {}
   		  )
   	       l
   	 fun first_false(i)=if Array.sub(arr,i) then first_false(i+1) else i
   	 val answer=(first_false(1) handle Subscript => list_length+1)
      in
   	 answer
      end

      fun greedy(G:('n,'a) graph,node_list)=let
   	 (* first thing we do is map the graph to one of int option refs,
            which contains the colouring as it proceeds, with NONE
            indicating no colour assigned yet (it would be more efficient to
            use 0 for that but I shall nobly resist the temptation) *)
   	 val g=arcNodeMap
   		 (fn arcLabel=>{})
   		 (fn nodeLabel=>ref (NONE:int option))
   		  G
         val internal_node_list=
            List.map
               (fn node=>valOf(internalise(g,node)))
               node_list
         val answer=List.map
(* we map each node in node_list to its colour, and as a side-effect
   alter the colour of the node *)
           (fn inode=>
               let
            	  val blocked_colours=
            	     List.mapPartial
            	       (fn inode2=> !(internal_nodeLabel inode2))
            	       (internal_neighbours inode)
            	  val colour=first_not_in blocked_colours
            	  val _= (internal_nodeLabel inode):=SOME colour
               in
                  (externalise(inode),colour)
               end
               )
            internal_node_list
      in
         answer
      end

      fun mindeg_order(G:('n,'a) graph)=
      let
      (* mindeg_order computes a list of the nodes in the order
      	 required to feed them into the greedy algorithm for
      	 mindeg_greedy.

      	 The basic idea is as follows.  We do not delete nodes of minimum
      	 degree; instead we simulate this by keeping a vector of dlists,
      	 where the dlist in entry i contains the inodes with degree i.
      	 When we "delete" a node, we remove it from its dlists, and
      	 bump each neighbour down from its dlist to the one below.
      	 *)
      	 val dlist_vec=Vector.tabulate(size(G),fn _=>DList.empty())
      	 (* Assuming that G is simple, no vertex can have degree >=size(G) *)

      	 (* We have a circularity problem to solve, since we want inodes
      	    to refer to ditems and vice-versa.  We solve this by making the
      	    ditems point to inode option ref, initially NONE, and then
      	    go over the new graph setting all the refs.  The inodes
            also contain a ref to their internal degree, or -1 if the
            inode has been deleted.  We also need a datatype declaration,
            to avoid a recursive type. *)

         datatype inode_label=L of
            ((inode_label,unit) internal_node option ref) DList.item
           *int ref


         val g=arcNode'Map
           (fn arcLabel=>())
           (fn inode=>
            let
               val deg=internal_degree(inode)
            in
               L(DList.add_right(
                  Vector.sub(dlist_vec,deg),
                  ref NONE
                  )
               ,
               ref deg
               )
            end)
            G

         fun inode_of_item item=valOf(!(DList.valof item))
         fun item_of_inode inode=
         let
            val L(item,_)=internal_nodeLabel inode
         in
            item
         end

         (* set all the refs correctly *)
         val _= Graph.internal_app
           (fn inode=>
               DList.valof(item_of_inode inode):=SOME inode
            )
            g

         fun bump_down(inode)=
         let
            val L(item,degr)=internal_nodeLabel(inode)
            val old_deg= !degr
            val _=if old_deg>0 then
            let
               val new_deg=old_deg-1
               val _= degr:=new_deg
               val _= DList.transfer_left(Vector.sub(dlist_vec,new_deg),item)
            (* it isn't specified which of the nodes with minimal degree
               gets deleted first.  Using transfer_left, together with
               leftmost later on, means there is a bias
               towards ones which were recently bumped down.  I think
               this is probably the best way, since in a homogenous graph
               vertices in the same patch will tend to get coloured together.
               *)
            in
               {}
            end
            else
 (* deg is 0 (in which case the graph is not simple) or -1, and
    the node has been deleted *)
               {}
         in
            {}
         end

         fun delete(item)=let
            val inode=inode_of_item item
            val L(_,degref)=internal_nodeLabel(inode)
            val _= degref:= ~1 (* set degree to -1 *)
            val _= DList.delete(item)
            val nbrs=internal_neighbours(inode)
            val _ =
               List.app
                  bump_down
                  nbrs
         in
            {}
         end

         fun do_rest(so_far,i)=
            if i>=Vector.length(dlist_vec)
            then
               so_far
            else
(* Assuming that no remaining vertex has degree <=i, order the remaining
   nodes and append them, backwards, to so_far *)
              (case DList.leftmost(Vector.sub(dlist_vec,i)) of
            	  SOME item=>
            	     let
            		val _ = delete(item)
            	     in
            		do_rest(externalise(inode_of_item item)::so_far,
                        Int.max(i-1,0))
            		(* If the minimum degree before deleting the node is
            		   i, it is certainly >= i-1 afterwards.  For
                           the worst case, consider the complete graph *)
            	     end
               |  NONE=>do_rest(so_far,i+1)
               )
      in
         do_rest([],0)
      end

      fun mindeg_greedy(G)=greedy(G,mindeg_order(G))
   end (* local *)
end (* struct *)
   	
