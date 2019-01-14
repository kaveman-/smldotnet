(*======================================================================*)
(* General tree datatype and automated layout functions.		*)
(* Adapted from:                                                        *)
(*    "Functional Pearls: Drawing Trees", A. J. Kennedy,                *)
(*    Journal of Functional Programming 6(3), May 1996, pp 527-534      *)
(* with the following modifications:                                    *)
(*   1. some functions (e.g. mergelist) are eta-expanded to comply with *)
(*      the `value restriction' of SML'97.                              *)
(*   2. functions from the new Standard Basis are used.                 *)
(*   3. the design function takes a function as argument that           *)
(*      determines the extent of a node label. This is used to          *)
(*      calculate the minimum separation between extents.               *)
(*   4. extents include heights as well as left and right limits.       *)
(*======================================================================*)
structure Tree :> Tree =
struct

(*----------------------------------------------------------------------*)
(* Trees with node data type 'a  					*)
(* Positioned trees have the type ('a * real) Tree, where 'a is a node  *)
(* label (e.g. string) and the real value is a horizontal position      *)
(* measured relative to the parent node.                                *) 
(*----------------------------------------------------------------------*)
datatype 'a Tree = Node of 'a * ('a Tree list)

(*----------------------------------------------------------------------*)
(* Displace a subtree by x'			                        *)
(*----------------------------------------------------------------------*)
fun movetree (Node((label,x), subtrees), x':real) = 
  Node((label, x+x'), subtrees)

(*----------------------------------------------------------------------*)
(* An Extent records left and right horizontal limits and height.       *) 
(* An Extent list for an entire tree gives the extent of each           *)
(* generation with absolute left and right limits.                      *)
(*----------------------------------------------------------------------*)
type Extent = { left : real, right : real, height : int }

(*----------------------------------------------------------------------*)
(* Displace a tree extent horizontally.					*)
(*----------------------------------------------------------------------*)
fun moveextent (e : Extent list, x) = 
  map (fn {left,right,height} => 
    {left=left+x,right=right+x,height=height}) e

(*----------------------------------------------------------------------*)
(* Merge two tree extents.						*)
(*----------------------------------------------------------------------*)
fun merge ([], qs) = qs
  | merge (ps, []) = ps
  | merge ({left=left, right=_,     height=h1}::ps, 
           {left=_,    right=right, height=h2}::qs) = 
          {left=left,  right=right, height=Int.max(h1,h2)} :: merge (ps,qs)

(*----------------------------------------------------------------------*)
(* Merge several extents						*)
(*----------------------------------------------------------------------*)
fun mergelist es = foldr merge [] es

(*----------------------------------------------------------------------*)
(* Determine minimum separation permitted between two extents		*)
(*----------------------------------------------------------------------*)
fun fit ({left=_,    right=right, height=_}::ps) 
        ({left=left, right=_,     height=_}::qs) = 
    Real.max(fit ps qs, right - left)

  | fit _ _ = 0.0

(*----------------------------------------------------------------------*)
(* Fit a list of extents together, working from the left; return a	*)
(* list of positions.                                                   *)
(*----------------------------------------------------------------------*)
fun fitlistl es = 
let
  fun fitlistl' acc [] = []
    | fitlistl' acc (e::es) = 
      let val x = fit acc e
      in
        x :: fitlistl' (merge (acc, moveextent (e,x))) es
      end
in
  fitlistl' [] es
end

(*----------------------------------------------------------------------*)
(* Fit a list of extents together, working from the right.		*)
(*----------------------------------------------------------------------*)
fun fitlistr es = 
let
  fun fitlistr' acc [] = []
    | fitlistr' acc (e::es) = 
      let val x = ~(fit e acc)
      in
        x :: fitlistr' (merge (moveextent (e,x), acc)) es
      end
in
  rev (fitlistr' [] (rev es))
end

(* or use the following functional-style trick!
val flipextent = map (fn {left, right, height}:Extent => 
                         {left = ~right, right = ~left, height = height})
val fitlistr = rev o (map ~) o fitlistl o map flipextent o rev
*)

(*----------------------------------------------------------------------*)
(* Fit a list of extents together symmetrically (fit from left and      *)
(* from right and then take the average).                               *)
(*----------------------------------------------------------------------*)
fun mean (x,y) = (x+y) / 2.0
fun fitlist es = ListPair.map mean (fitlistl es, fitlistr es)

(*----------------------------------------------------------------------*)
(* Given a labelled tree, determine a pleasing layout. Return the      	*)
(* positioned tree and its extent.                                      *)
(*----------------------------------------------------------------------*)
fun design (tree, extentf) =
let
  fun design' (Node(label, subtrees)) =
  let
    val (trees, extents) = ListPair.unzip (map design' subtrees)
    val positions = fitlist extents
    val ptrees = ListPair.map movetree (trees, positions)
    val pextents = ListPair.map moveextent (extents, positions)
    val resultextent = extentf label :: mergelist pextents
    val resulttree = Node((label,0.0), ptrees)
  in
    (resulttree, resultextent)
  end
in
  design' tree
end

(*----------------------------------------------------------------------*)
(* Return the leftmost and rightmost position in an extent		*)
(*----------------------------------------------------------------------*)
fun collapseExtent [] = {left = 0.0, right = 0.0, height = 0}
  | collapseExtent ({left,right,height}::rest) = 
    let val {left=left',right=right',height=height'} = collapseExtent rest
    in
      {left = Real.min(left,left'), right = Real.max(right,right'),
       height = height+height'}
    end


end

