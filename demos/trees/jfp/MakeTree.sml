(*======================================================================*)
(* A variety of tree generators						*)
(*======================================================================*)
structure MakeTree :> MakeTree =
struct

local open Tree in

(*----------------------------------------------------------------------*)
(* Generate a king or queen's name                                      *)
(*----------------------------------------------------------------------*)
val kingsandqueens =
  ["Charles", "George", "Henry", "Edward", "William", "Richard", "Victoria",
   "Elizabeth", "Anne", "James", "Mary", "Alfred", "Ethelred",
   "Stephen", "Matilda", "David", "Harold"]
val royalcount = length kingsandqueens

fun kingorqueen m =
List.nth(kingsandqueens, Int.rem(m, royalcount))

fun node (n,ts) = Node(kingorqueen n, ts)
fun leaf n = node (n,[])

fun leaves 0 = []
  | leaves n = leaf n :: leaves (n-1)

fun chain' (count, 0) = leaf count
  | chain' (count, n) = node(count, [chain' (count+1, n-1)])

fun chain n = chain' (0,n)

(*----------------------------------------------------------------------*)
(* Generate a `pathological' tree of depth n as in Fig. 6 in the JFP    *)
(* article.                                                             *)
(*----------------------------------------------------------------------*)
fun pathol n =
let
  fun pathol' (count, 0) = leaf count
    | pathol' (count, n) = node(count, 
      [pathol' (count+n+1, n-1), chain' (count+1, n-1)])
in
  pathol' (0, n)
end


(*----------------------------------------------------------------------*)
(* Generate a full binary tree of depth n				*)
(*----------------------------------------------------------------------*)
fun fullbinary n =
let
  fun fullbinary' count 0 = leaf (count-1)
    | fullbinary' count n = 
      node(count-1, 
        [fullbinary' (count*2) (n-1), fullbinary' (count*2+1) (n-1)])
in
  fullbinary' 1 n
end


(*----------------------------------------------------------------------*)
(* Given a random number generator, create a random tree where the      *)
(* number of children for the i'th generation (counting from 0) is      *)
(* determined by a function f(i,r) where r is a random number between   *)
(* 0 and 1.                                                             *)
(*----------------------------------------------------------------------*)
fun make f =
let
  fun random' (count, level, 0) = (count, [])
    | random' (count, level, n) =
      let 
        val m = f level
        val (count', subtrees) = random' (count+1, level+1, m)
        val (count'', siblings) = random' (count', level, n-1)
      in
        (count'', node (count, subtrees) :: siblings)
      end  
  val (_, [t]) = random' (0, 0, 1)
in 
  t
end

fun count (Node(_,nodes)) = foldl op+ 1 (map count nodes)
    
end

end

