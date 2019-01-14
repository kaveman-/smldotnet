(*======================================================================*)
(* Miscellaneous stuff to do with list types				*)
(*======================================================================*)
structure ListOps =
struct

fun singleton x = [x]

fun mapfoldl f e []      = ([], e)
  | mapfoldl f e (x::xs) = 
    let
      val (x', e') = f (x, e)
      val (xs', r) = mapfoldl f e' xs
    in
      (x'::xs', r)
    end

(*----------------------------------------------------------------------*)
(* Map, app, foldr and foldl with position in list			*)
(*----------------------------------------------------------------------*)
fun mapi f xs =
let fun map' n [] = []
      | map' n (x::xs) = f(n,x) :: map' (n+1) xs
in
  map' 0 xs
end

fun appi f xs =
let fun app' n [] = ()
      | app' n (x::xs) = (f(n,x); app' (n+1) xs)
in
  app' 0 xs
end

fun foldri f acc xs = 
  let val (result,_) = 
    foldr (fn (x,(acc,n)) => (f (n-1,x,acc), n-1)) (acc,length xs) xs
  in result end

fun foldli f acc xs = 
  let val (result,_) = 
    foldl (fn (x,(acc,n)) => (f (n,x,acc), n+1)) (acc,0) xs
  in result end

val sum = foldr op+ 0
val max = foldr Int.max 0

end
