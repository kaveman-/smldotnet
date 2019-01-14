(* StrongComponents:STRONGCOMPONENTS finds the strongly connected components
   of a graph.

   The algorithm used is a fairly straight translation of that in
   Fig 1.20 of Gibbons, "Algorithmic Graph Theory" (1985).  It would probably
   not be easy to understand it without reference to the book.  The only
   changes are that we avoid array references and instead use refs attached
   to nodes of the graph.

   It can be verified from Gibbons that strong_components does indeed output
   the strongly connected components of the graph.  However, the order
   constraint specified in the STRONGCOMPONENTS signature is not mentioned
   there.  We justify it as follows.  First, note that the
   ML code,
      "components_so_far:=poptov(v):: !components_so_far"
   which outputs a component actually prepends it to the list
   components_so_far, so that the components are in the list in the reverse
   order to that in which they were found.  However, by the argument
   in Gibbons, dfsscc is equivalent to df in the following pseudocode:

      fun df(v)=
 1)      visit v
 2)      call df for all unvisited vertices v' where v->v' is an arc
 3)      if v is the root of a strongly connected component, prepend that
            component to the list

   Suppose C1 and C2 are strongly connected components, with a path
   from C1 to C2; let v1 and v2 be the roots of C1 and C2; then there
   is a path from v1 to v2.  We need to show that C2 is prepended to the
   list before C1.  If df(v2) is called before df(v1), this is certainly
   true.  Otherwise v2 must be visited at line 2 of df(v1), and C2 is
   prepended then, which is before line 3 of df(v1), when C1 is prepended.

   *)

structure StrongComponents:>STRONGCOMPONENTS=
struct
   open Graph

   (* We operate on a transformed graph with unit arc labels and
      node labels of type node_data: *)
      type node_data={
         dfi:int ref, (* index in depth-first search, corresponds to
                         DFI in Gibbons.  We also maintain a
                         set "unvisited of unvisited nodes*)
         q:int ref, (* corresponds to Q in Gibbons *)
         stacked:bool ref (* stacked is true if this node is on the
                       stack (so corresponds to stacked in Gibbons *)
         }

   (* We need sets of internal components; we do this using Graph.OrdNode
      and the splay tree functor.  Fortunately the transformed graph
      is monomorphic, and all internal nodes are of type inode: *)
   type inode=(node_data,unit) internal_node

   (* the following functions access dfi,q and stacked. *)
   fun dfi(n:inode)= #dfi(internal_nodeLabel(n))
   fun q(n:inode)= #q(internal_nodeLabel(n))
   fun stacked(n:inode)= #stacked(internal_nodeLabel(n))

   structure OrdInternalNode:>ORD_KEY where type ord_key=inode =
   struct
      type ord_key=inode
      fun compare(inode1,inode2)=
         OrdNode.compare(externalise(inode1),externalise(inode2))
   end

   structure INS=SplaySetFn(OrdInternalNode)
   (* INS stands for InternalNodeSet *)

   fun strong_components(G:('n,'a) graph)=let
      (* Our first action is to transform the graph into one in which
         the node and arc labels have been thrown away and been replaced
         by node_data labelling each node. *)
      val g:(node_data,unit) graph=arcNodeMap (fn arclabel=>{})
        (fn nodelabel=>{dfi=ref 0,q=ref 0,stacked=ref false})
         G

      (* We now provide some horrible imperative variables corresponding
         to the ones in Gibbons. *)

(* we maintain two representations for the stack: "stack, which is
   a ref list containing the stack itself, and "stacked, which is
   a field of node_data which is set to true if the node is stacked. *)
      local
         val stack=ref ([]:inode list)
      in
         fun push(v:inode)=(
            stack:= v:: !stack;
            stacked(v):=true
            )

         fun is_stacked(v:inode)= !(stacked v)

         (* poptov pops to v, its argument, and returns the list of
            resulting vertices, in undefined order,
           (this is how components are obtained;
            see line 11 of Gibbons algorithm) *)
         fun poptov(v:inode)=let
            fun ppv(sofar,[])=
               raise Fail("ppv called for unstacked vertex!")
            |   ppv(sofar,hd::tl)=
               let

                  (* change hd's stacked flag *)
                  val _= stacked(hd):=false
                  val nextsofar=hd::sofar
               in
                  if is_equal(v,hd)
                  then
                      (nextsofar,tl)
                  else
                      ppv(nextsofar,tl)
               end
            val (result,new_stack)=ppv([],!stack)
            val _ = stack:=new_stack
         in
            result
         end
      end (* that ends basic stack operations *)

(* we also need to repeatedly find the next unvisited vertex.  Visited
   vertices can be distinguished because their DFI field is 0.  Once
   a vertex is visited, it never becomes unvisited.  get_nextunvisited
   does the trick. *)

      local
         val unchecked=ref (internal_nodeList(g))
      in
         fun get_nextunvisited ()=
           (case !unchecked of
               [] => NONE
            | hd::tl =>
               let
                  val _ = unchecked:=tl
               in
                  if !(dfi hd)=0
                  then
                     SOME hd
                  else
                     get_nextunvisited()
               end
            )
      end

      val i=ref 1; (* this is the depth-first-search counter *)

      val components_so_far=ref([]:inode list list)
      (* components_so_far contains the components we have found *)
(* Now for dfsscc itself, which is translated directly from DFSSCC in
   Gibbons.  *)
      fun dfsscc(v:inode):unit=
      let
         val _= dfi v:= !i
         val _= q v:= !i
         val _= i:= !i+1
         val _= push(v)
         val nbrs=internal_neighbours(v)
         val _= List.app
           (fn v'=>
               if !(dfi v')=0
               then
                 (dfsscc(v');
                  q(v):= Int.min(!(q v),!(q v'))
                  )
               else
                 if !(dfi v')< !(dfi v) andalso is_stacked(v')
                 then
                    q(v):= Int.min(!(q v),!(dfi v'))
                 else
                    {}
            )
            nbrs
         val _=
            if !(q v)= !(dfi v)
            then
               components_so_far:=poptov(v):: !components_so_far
            else
               {}
      in
         {}
      end

(* now for the loop that repeatedly calls dfsscc *)
      fun do_all_remaining()=(case get_nextunvisited() of
         SOME v=>
           (dfsscc(v);
            do_all_remaining())
      |  NONE  =>{}
         )

      (* find the components! *)
      val _=do_all_remaining();
   in
      (* !components_so_far contains the components but we have to
         externalise them *)
      List.map (List.map externalise) (!components_so_far)
   end
end (* struct *)
