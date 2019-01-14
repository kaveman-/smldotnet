(*======================================================================*)
(* Operators to lift equality operations to parameterised datatypes	*)
(*======================================================================*)
structure Eq :> EQ = 
struct

fun list eq ([],[]) = true
  | list eq (x::xs,y::ys) = eq(x,y) andalso list eq (xs,ys)
  | list eq _ = false

fun option eq (NONE,NONE) = true
  | option eq (SOME x, SOME y) = eq(x,y)
  | option eq _ = false

(*----------------------------------------------------------------------*)
(* Are two lists equal, irrespective of order?				*)
(*----------------------------------------------------------------------*)
fun unordered eq (xs,ys) =
let
  fun remove x [] = NONE
    | remove x (y::ys) = 
      if eq(x,y) then SOME ys
      else 
        case remove x ys of
          NONE => NONE
        | SOME ys' => SOME (y::ys')

  fun check ([], []) = true
    | check ([], _) = false
    | check (x::xs, ys) = 
      case remove x ys of
        NONE => false
      | SOME ys' => check (xs, ys')
in
  check (xs,ys)
end

end
