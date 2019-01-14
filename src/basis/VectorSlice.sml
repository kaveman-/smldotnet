structure VectorSlice :> VECTOR_SLICE =
struct

local 
  open General Option PrimUtils_.Int 
in

type 'a slice = 'a vector * int * int

fun length (v,i,len) = len
fun sub ((v,i,len),j) = Vector.sub(v,i+j)

fun slice (v,i,NONE) = if i < 0 orelse i > Vector.length v then raise General.Subscript else (v, i, Vector.length v - i)
  | slice (v,i,SOME len) = if i < 0 orelse len < 0 orelse i+len > Vector.length v then raise General.Subscript else (v,i,len)

fun full v = (v,0,Vector.length v)

fun base (p:'a slice) = p
fun isEmpty ((v,i,0):'a slice) = true
  | isEmpty _ = false

fun subslice ((v,i,n),j,mopt) = raise General.Fail "VectorSlice.subslice not implemented yet"

fun vector (v,0,len) = if Prim.=(len,Vector.length v) then v else Vector.tabulate (len,fn i => Vector.sub(v,i))
  | vector slice = Vector.tabulate (length slice,fn j => sub(slice,j))

fun foldli f e (slice as (a, i, n)) = 
    let fun loop stop =
	    let fun lr j res = 
		if j < stop then lr (j+1) (f(j, Vector.sub(a,j), res))
		else res
	    in lr i e end
    in loop (i+n) end

fun foldri f e (slice as (a, i, n)) = 
    let fun loop start =
	    let fun rl j res = 
		    if j >= i then rl (j-1) (f(j, Vector.sub(a,j), res))
		    else res
	    in rl start e end;
    in loop (i+n - 1) end

fun appi f (slice as (a, i, n)) = 
    let fun loop stop = 
	    let	fun lr j = 
		    if j < stop then (f(j, Vector.sub(a,j)); lr (j+1)) 
		    else ()
	    in lr i end
    in loop (i+n) end

fun mapi (f : 'a -> 'b) (a, i, n) =
    let
      val newvec : 'a array = Prim.newarray(n)
      fun copy j = 
        if Int.<(j,n)
        then (Prim.arraystore(newvec, j, f(i+j, Vector.sub(a,i+j))); copy (j+1))
        else Prim.toVector newvec
    in 
      copy 0 
    end



fun app f sl = appi (f o #2) sl
fun map f sl = mapi (f o #2) sl

fun foldl f init sl = foldli (fn (_, a, x) => f(a, x)) init sl
fun foldr f init sl = foldri (fn (_, a, x) => f(a, x)) init sl

fun all _ _ = raise Fail "VectorSlice.all not implemented"
fun exists _ _ = raise Fail "VectorSlice.exists not implemented"
fun find _ _ = raise Fail "VectorSlice.find not implemented"
fun findi _ _ = raise Fail "VectorSlice.findi not implemented"
fun collate _ _= raise Fail "VectorSlice.collate not implemented"
fun concat _ = raise Fail "VectorSlice.concat not implemented"
fun getItem _ = raise Fail "VectorSlice.getItem not implemented"
end

end