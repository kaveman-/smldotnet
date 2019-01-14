(* Graph:GRAPH contains basic code for graph manipulation.  Crudely, a
   graph is a set of labelled nodes, to each of which is attached a set of
   labelled arcs going to other nodes.  The set of arcs leaving a given node
   can be found rapidly, but the arcs going to a node cannot; if it's
   desired to have a datastructure containing this information, it will be
   necessary to construct a second graph, where the arcs are reversed.

   Designing the interface was fairly tricky.  I have tried to attain the
   following objectives:

   1) the functions should be side-effect free and pure.  This is true with
      the exception of the new_node function
   2) the code should not be grossly inefficient (extra log factors are
      OK but not turning linear into quadratic time)
   3) the functions can be reimplemented later on, for example to make
      them much faster
   4) it should be possible to construct more than one graph for a given
      set of nodes

   To allow several graphs on the same set of nodes, we provide
   both nodes and internal_nodes.  internal_nodes are effectively
   pointed graphs and there is a map which takes a node and a graph
   containing it to the associated pointed graph.  This map takes some time
   so it is more efficient to use internal_nodes.

   It is not possible currently to alter graphs on the fly, because this
   is tricky to do efficiently without functions having side-effects.  In
   any case, I can't think of an algorithm I shall want to implement which
   needs this; where algorithms do make changes to graphs (such as
   the colouring algorithm which repeatedly deletes vertices of minimum
   degree from graphs) they can be recoded more efficiently in other ways.
   In any case, it should not be hard to add additional side-effecting
   functions later on which do modify the graph structure.

   In the implementation we use ATT IntBinaryMap's; this is nonstandard but
   they can be expected to be commonly implemented and in any case they can
   be re-implemented in the standard language.
   *)
structure Graph:>GRAPH=
struct
   (* the datastructure used is as follows.  Nodes are ints.
      Graphs and arc lists themselves are IntBinaryMaps keyed by the
      node number (as given by node, so nodes are not only used externally).
      The Graph IntBinaryMap maps to internal nodes.  The arc list
      IntBinaryMap maps to a tuple of an arc label and a ref to an internal
      node.  internal nodes are a tuple of a node label and an arc list.
      *)

   structure IBM=IntBinaryMap (* this is just an abbreviation *)

   datatype node=N of int

   datatype ('n,'a) internal_node=I of {
      label:'n, (* this is the label of the node *)
      node:node, (* this is the corresponding node *)
      arcs: ('a*(('n,'a) internal_node)) IBM.map ref
      }

   datatype ('n,'a) graph=G of ('n,'a) internal_node IBM.map

   fun internal_nodeLabel(I{label,...})=label

   fun internalise(G m,N n)=IBM.find(m,n)
   fun externalise(I inode)= #node(inode)

   (* common_arcLabel does arcLabel when from is an internal_node and
      to is a node. *)
   fun common_arcLabel(I {arcs,...},N i)=
      let
         val result=IBM.find(!arcs,i)
      in
         case result of
            NONE => NONE
         |  SOME (a,_) => SOME a
      end

   fun internal_arcLabel {from,to}=common_arcLabel(from,externalise(to))

   fun internal_neighbours(I {arcs,...})=
      List.map
        (fn (alab,inode)=>inode)
        (IBM.listItems (!arcs))

   fun internal_degree(I {arcs,...})=
      IBM.numItems(! arcs)

   fun is_equal(I{node=N n1,...},I{node=N n2,...})=(n1=n2)

   val nodeSupply=ref 0

   fun newNode {}=let
      val _ = nodeSupply:= !nodeSupply +1
   in
      N(!nodeSupply)
   end

   datatype ('n,'a) preGraph=pre of {
      nodes: {node:node,label:'n} list,
      arcs: ({from:node,to:node}*'a) list
      }

   fun makeGraph(pre {nodes,arcs})=let
      val graph as G(graphmap)=G(List.foldl
        (fn ({node as N node_number,label},gsofar) =>
         IBM.insert(gsofar,node_number,
            I{label=label,node=node,arcs=ref IBM.empty}))
         IBM.empty
         nodes
         )
      (* graph will be the result but we need to add the arcs to it *)
      val _=List.app
        (fn ({from,to as N to_number},arclabel)=>let
            val _= (case internalise(graph,from) of
               NONE => raise Fail(
"makeGraph called with arc from node not in node list")
            |  SOME(I{arcs,...})=>
               let
                  val oldarcs= !arcs
                  val newarcs=(case internalise(graph,to) of
                     NONE => raise Fail(
"makeGraph called with arc to node not in node list")
                  |  SOME to_internal =>
                     IBM.insert(oldarcs,to_number,(arclabel,to_internal))
                     ) (* end of case *)
               in
                  arcs:=newarcs
               end
               ) (* end of case *)
            in {}
            end
         ) (* end of fn *)

         arcs
        (* end of List.foldl *)
      in
         graph
      end (* end of makeGraph *)

   fun size(G M)=IBM.numItems M

   fun nodeLabel(g,node)=
     (case internalise(g,node) of
         SOME inode=>SOME(internal_nodeLabel(inode))
      |  NONE=>NONE
      )

   fun arcLabel(g,{from,to})=
     (case (internalise(g,from),internalise(g,to)) of
         (SOME f,SOME t)=>common_arcLabel(f,to)
      |  _              => NONE
      )

   fun nodeList(G M)=
      IBM.foldl
         (fn (I{node,...},sofar)=>node::sofar)
         []
         M

   fun internal_nodeList(G M)=
      IBM.foldl
         (fn (inode,sofar)=>inode::sofar)
         []
         M

   fun neighbours(g,node)=
     (case internalise(g,node) of
         NONE => NONE
      |  SOME nd=>SOME(
         List.map
            externalise
           (internal_neighbours nd)
         )
      )

   fun degree(g,node)=
     (case internalise(g,node) of
         NONE => NONE
      |  SOME nd=>SOME(internal_degree nd)
      )

   structure OrdNode=
   struct
      type ord_key=node
      fun compare(N n1,N n2)=Int.compare(n1,n2)
   end

   fun internal_app f (G M)=IBM.app f M

   (* G oldmap and G newmap should be graphs with the same nodes.
      arcNode_copy_arcs replaces the arcs of G newmap by the arcs
      of G newmap, transforming the labels by g. *)
   fun arcNode_copy_arcs (g:'a1->'a2)
     {from as (G oldmap):('n1,'a1) graph,to as (G newmap):('n2,'a2) graph}=
   let
      fun to_old(inode)=let (* to_old finds the corresponding old node,
                             given the new node *)
         val old_node=
        (case internalise(G oldmap,externalise(inode)) of
            SOME n => n
         |  NONE   => raise Fail("to_old internal error")
            )
      in
         old_node
      end

      fun to_new(inode)=let (* to_new finds the corresponding new node,
                             given the old node *)
         val new_node=
        (case internalise(G newmap,externalise(inode)) of
            SOME n=>n
         |  NONE  =>raise Fail("to_new internal error")
         )
      in
         new_node
      end

      val _=IBM.app
        (fn new as I {label,node,arcs}=>
            arcs := IBM.map
               (fn (arclabel,inode)=>(g (arclabel:'a1),to_new(inode)))
               let
                  val I{arcs=oldarcs,...}=to_old(new)
               in
                  !oldarcs
               end
         )
         newmap
   in
      {}
  end

   fun arcNodeMap (g:'a1->'a2) (f:'n1->'n2) (G oldmap)=let
      (* We do this in two passes: first we construct the nodes (with
         no arc lists), then we set the arc list for each one. *)
      val newmap=IBM.map
         (fn I {label,node,arcs}=>
            I {label=f(label),node=node,arcs=ref(IBM.empty)})
         oldmap
      val _= arcNode_copy_arcs g {from=G oldmap,to=G newmap}
   in
      G newmap
   end

   fun arcNode'Map g f (G oldmap)=let
      (* This is similar to arcNodeMap *)
      val newmap=IBM.map
         (fn inode as I {node,arcs,...}=>
            I {label=f(inode),
               node=node,arcs=ref(IBM.empty)})
         oldmap
      val _= arcNode_copy_arcs g {from=G oldmap,to=G newmap}
   in
      G newmap
   end

   fun nodeMap f=arcNodeMap (fn arclabel=>arclabel) f
end
