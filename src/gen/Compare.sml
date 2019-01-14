(*======================================================================*)
(* Operators to lift comparison operations to parameterised datatypes	*)
(*======================================================================*)
structure Compare :> COMPARE =
struct

fun list c ([],[]) = EQUAL
  | list c ([],y::ys) = LESS
  | list c (x::xs,[]) = GREATER
  | list c (x::xs,y::ys) = 
    case c (x,y) of
      EQUAL => list c (xs,ys)
    | other => other

fun option c (NONE,NONE) = EQUAL
  | option c (SOME x, SOME y) = c(x,y)
  | option c (NONE, SOME _) = LESS
  | option c (SOME _, NONE) = GREATER

fun pair (c,d) ((x,y),(w,z)) =
  case c (x,w) of
    EQUAL => d (y,z)
  | other => other

end
