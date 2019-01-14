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
signature GRAPH=
sig
   eqtype node
   val newNode:{}->node (* impure and with side-effects *)
   type ('n,'a) graph
   (* 'n is the type of node labels; 'a is the type of arc labels *)

   datatype ('n,'a) preGraph=pre of {
      nodes: {node:node,label:'n} list,
      arcs: ({from:node,to:node}*'a) list
      }

   val makeGraph: ('n,'a) preGraph->('n,'a) graph

   val size: ('n,'a) graph->int
   (* N is the number of nodes in the graph *)

   val nodeLabel: ('n,'a) graph*node->'n option
   (* nodeLabel looks up the node in the graph and returns SOME l if there
      is one with label l, otherwise NONE. *)
   val arcLabel: ('n,'a) graph*{from:node,to:node}->'a option
   (* arcLabel returns SOME a if there are nodes from and to in the graph
      and an arc going from "from to "to with label a, otherwise NONE. *)
   val nodeList: ('n,'a) graph->node list
   (* nodeList returns the list of nodes in a graph *)
   val neighbours: ('n,'a) graph * node->node list option
   (* neighbours returns the neighbours of the given node in a graph, or
      NONE if the node does not belong to the graph *)
   val degree: ('n,'a) graph*node->int option
   (* degree returns the degree of the node in the graph, or NONE if the node
      does not belong to the graph *)
   structure OrdNode:ORD_KEY where type ord_key = node
   (* OrdNode is an ORD_KEY structure which can be used to construct
      dictionaries and sets on nodes, using the splay tree functor. *)

   val nodeMap: ('n1->'n2)->('n1,'a) graph->('n2,'a) graph
   (* nodeMap transforms a graph, changing all the node labels *)

   val arcNodeMap: ('a1->'a2)->('n1->'n2)->('n1,'a1) graph->('n2,'a2) graph
   (* arcNodeMap transforms a graph, changing all the node labels and
      arc labels *)

(* Now for the internal operations.  These are faster but maybe not so
   convenient.  There is no way of constructing a graph just in terms of
   internal_nodes, but there's no reason why you can't throw away the nodes
   once the graph is constructed and use internal_nodes for everything.
   *)

   type ('n,'a) internal_node
   val internalise:('n,'a) graph*node->('n,'a) internal_node option
   (* lookup constructs the internal_node corresponding to the node in the
      supplied graph, or returns NONE if there is none. *)
   val externalise:('n,'a) internal_node->node
   (* externalise recovers the node from the corresponding internal_node
      (there is no way of obtaining the graph containing a given
      internal_node).  externalise should actually be very fast *)

   (* the following functions, of the form internal_XXXX, are similar to
      the corresponding XXXX functions, but use internal_nodes rather than
      nodes, do not need the graph, and have other minor differences
      specified in comments when they are defined. *)
   val internal_nodeLabel: ('n,'a) internal_node->'n
   (* This is like nodeLabel but returns 'n rather than 'n option as there
      should always be a label. *)
   val internal_arcLabel:
      {from:('n,'a) internal_node,to:('n,'a) internal_node}->'a option
   (* Like arcLabel (it returns NONE if "from and "to belong to different
      graphs, but this should not be relied upon). *)
   val internal_neighbours: ('n,'a) internal_node->('n,'a) internal_node list
   (* internal_neighbours is like neighbours, but returns internal_node list
      rather than internal_node list option since any internal_node
      should always be part of a graph.*)
   val internal_degree:('n,'a) internal_node->int
   (* internal_degree is like degree, but returns int rather than int option
      since any internal_node should always be part of a graph *)
   val internal_nodeList: ('n,'a) graph->('n,'a) internal_node list

   val is_equal:('n,'a) internal_node*('n,'a) internal_node->bool

   val internal_app:(('n,'a) internal_node->unit)->('n,'a) graph->unit
   (* internal_app is like app (the order of evaluation is unspecified) *)

   val arcNode'Map: ('a1->'a2)->(('n1,'a1) internal_node->'n2)->
      ('n1,'a1) graph->('n2,'a2) graph
   (* arcNode'Map is like arcNodeMap except the node mapping function
      gets the whole internal node, not just its label. *)

end
