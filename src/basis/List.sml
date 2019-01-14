structure List :> LIST =
struct

exception Empty
datatype list = datatype Datatypes.list

local 
  open General Option PrimUtils_.Int 
in

fun revAppend ([], ys) = ys
  | revAppend (x::xs, ys) = revAppend (xs, x::ys)

fun rev xs = revAppend (xs, [])

fun [] @ ys = ys
  | (x::xs) @ ys = x :: (xs @ ys)

fun null []       = true
  | null _        = false

fun hd []         = raise Empty
  | hd (x::xs)    = x

fun tl []         = raise Empty
  | tl (x::xs)    = xs

fun last []       = raise Empty
  | last [x]      = x
  | last (x::xs)  = last xs

fun nth (xs, n) =
let 
  fun nth' ([], _) = raise General.Subscript
    | nth' (x::xs, n) = case n of 0 => x | _ => nth'(xs, n-1)
in 
  if n < 0 then raise General.Subscript 
  else nth' (xs, n) 
end

fun drop (xs, n) =
let 
  fun drop' (xs, 0) = xs
    | drop' ([], n) = raise General.Subscript
    | drop' (x::xs, n) = drop' (xs, n-1)
in 
  if n<0 then raise General.Subscript 
  else drop' (xs, n) 
end

fun take (xs, n) =
let
  fun take' (_, 0) = []
    | take' ([],n) = raise General.Subscript
    | take' (x::xs,n) = x :: take' (xs,n-1)
in
  if n<0 then raise General.Subscript
  else take' (xs, n)
end

fun length xs =
let
  fun length' ([], k) = k
    | length' (x::xs, k) = length' (xs, k+1)
in 
  length' (xs, 0)
end

fun concat [] = []
  | concat (xs::xss) = 
    let
      fun concat' ([], xss) = concat xss
        | concat' (x::xs, xss) = x :: concat'(xs,xss)
    in
      concat' (xs,xss)
    end

fun app f []      = ()
  | app f (x::xs) = (f x; app f xs)

fun map f xs =
let
  fun map' [] = []
    | map' (x::xs) = f x :: map' xs
in
  map' xs
end

fun mapPartial f xs =
let
  fun mapPartial' [] = []
    | mapPartial' (x::xs) =
      case f x of
        NONE => mapPartial' xs
      | SOME y => y :: mapPartial' xs
in
  mapPartial' xs
end

fun find p []      = NONE
  | find p (x::xs) = if p x then SOME x else find p xs

fun filter p xs =
let
  fun filter' [] = []
    | filter' (x::xs) = if p x then x::filter' xs else filter' xs
in
  filter' xs
end

fun partition p xs =
let
  fun partition' [] = ([],[])
    | partition' (x::xs) =
      let
        val b = p x
        val (yes, no) = partition' xs
      in
        if b then (x::yes,no) 
             else (yes, x::no)
      end
in
  partition' xs
end

fun foldr f e []      = e
  | foldr f e (x::xs) = f(x, foldr f e xs)

fun foldl f e []      = e
  | foldl f e (x::xs) = foldl f (f(x, e)) xs

fun exists p []      = false
  | exists p (x::xs) = p x orelse exists p xs

fun all p []      = true
  | all p (x::xs) = p x andalso all p xs

fun tabulate (n,f) =
let
  fun tabulate' i = if i < n then f i :: tabulate' (i+1) else []
in
  if n<0 then raise General.Size
  else tabulate' 0
end

fun getItem [] = NONE
  | getItem (x::xs) = SOME (x,xs)

end

end

