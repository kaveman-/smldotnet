(*======================================================================*)
(* General tree datatype and automated layout functions.		*)
(*======================================================================*)
signature Tree =
sig

(* Each node has data of type 'a and a list of subtrees *)
datatype 'a Tree = Node of 'a * ('a Tree list)

(* The rectangular extent of a single node, generation, or tree *)
(* The left and right extents are relative to the root of the tree *)
type Extent = { left : real, right : real, height : int }

(* Given a tree and a function that determines the extent of each node
   relative to its parent, assign each node a horizontal position relative
   to its parent and return an extent for each generation in which left
   and right extents are relative to the root of the whole tree *)
val design : ('a Tree * ('a -> Extent)) ->  ('a * real) Tree * Extent list

(* Given the extents of each generation in a tree, determine the extent of
   the whole tree *)
val collapseExtent : Extent list -> Extent

end

