(*======================================================================*)
(* Tree-based lists, providing constant-time append			*)
(*======================================================================*)
structure TreeList :> TREELIST =
struct

datatype 'a treelist = 
  Empty 
| Single of 'a
| Join of 'a treelist * 'a treelist

val empty = Empty
fun cons(x,y) = Join(Single x, y)
fun null Empty = true | null _ = false

fun x @ y = Join(x,y)

fun fromList xs = foldr cons empty xs

fun concat xs = foldr op@ empty xs

fun rev Empty = Empty
  | rev (Single x) = Single x
  | rev (Join(x,y)) = Join(rev y, rev x)

fun length Empty = 0
  | length (Single _) = 1
  | length (Join(x,y)) = length x + length y

fun app f Empty = ()
  | app f (Single x) = f x
  | app f (Join(x,y)) = (app f x; app f y)

fun map f Empty = Empty
  | map f (Single x) = Single (f x)
  | map f (Join(x,y)) = Join(map f x, map f y)

fun toList Empty = [] 
  | toList (Single x) = [x]
  | toList (Join(x,y)) = List.@(toList x, toList y)

fun getItem Empty = NONE
  | getItem (Single x) = SOME (x, Empty)
  | getItem (Join(x,y)) = 
    case getItem x of
      NONE => getItem y
    | SOME (h,t) => SOME (h, Join(t,y))

end


