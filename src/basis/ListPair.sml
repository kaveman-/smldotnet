structure ListPair :> LIST_PAIR =
struct

local 
  open List Bool 
in

fun zip (x::xs, y::ys) = (x,y) :: zip(xs,ys)
  | zip _ = []

fun unzip [] = ([],[])
  | unzip ((x,y)::zs) = 
    let
      val (xs,ys) = unzip zs
    in
      (x::xs,y::ys)
    end

local
  fun unzip' ([],xs,ys) = (rev xs, rev ys)
    | unzip' ((x,y)::zs, xs, ys) = 
      unzip' (zs, x::xs, y::ys)
in
  fun unzip p = unzip' (p,[],[])
end

local 
  fun zip' (x::xs, y::ys, ps) = zip' (xs, ys, (x,y)::ps)
    | zip' (_, _, ps) = rev ps
in
  fun zip (xs, ys) = zip' (xs, ys, [])
end

fun map f (xs, ys) =
let
  fun map' (x::xs, y::ys) = f (x,y) :: map' (xs,ys)
    | map' (_, _) = []
in
  map' (xs,ys)
end

fun app f (xs, ys) = 
    let fun app' (x::xr, y::yr) = (f (x, y); app' (xr, yr))
	  | app' _              = ()
    in app' (xs, ys) end

fun all p (xs, ys) = 
    let fun h (x::xr, y::yr) = p(x, y) andalso h (xr, yr)
	  | h _              = true
    in h (xs, ys) end

fun exists p (xs, ys) = 
    let fun h (x::xr, y::yr) = p(x, y) orelse h (xr, yr)
	  | h _              = false
    in h (xs, ys) end

fun foldr f e (xs, ys) = 
    let fun h (x::xr, y::yr) = f(x, y, h (xr, yr))
	  | h _              = e
    in h (xs, ys) end

fun foldl f e (xs, ys) = 
    let fun h (e, x::xr, y::yr) = h (f(x, y, e), xr, yr)
	  | h (e, _,     _    ) = e
    in h (e, xs, ys) end

end

end
