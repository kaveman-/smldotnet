structure ArraySlice :> ARRAY_SLICE =
struct

local 
  open General Option PrimUtils_.Int 
in

type 'a slice = 'a array * int * int

fun length (a,i,len) = len
fun sub ((a,i,len),j) = Array.sub(a,i+j)
fun update((a,i,len),j,x) = Array.update(a,i+j,x)

fun slice (a,i,NONE) = if i < 0 orelse i > Array.length a then raise General.Subscript else (a, i, Array.length a - i)
  | slice (a,i,SOME len) = if i < 0 orelse len < 0 orelse i+len > Array.length a then raise General.Subscript else (a,i,len)

fun full a = (a,0,Array.length a)

fun base (p:'a slice) = p
fun vector sl = Vector.tabulate (length sl, fn i => sub (sl, i))

fun isEmpty ((a,i,0):'a slice) = true
  | isEmpty _ = false

fun subslice ((a,i,n),j,mopt) = raise General.Fail "ArraySlice.subslice not implemented yet"

fun foldli f e (slice as (a, i, n)) = 
    let fun loop stop =
	    let fun lr j res = 
		if j < stop then lr (j+1) (f(j, Array.sub(a,j), res))
		else res
	    in lr i e end
    in loop (i+n) end

fun foldri f e (slice as (a, i, n)) = 
    let fun loop start =
	    let fun rl j res = 
		    if j >= i then rl (j-1) (f(j, Array.sub(a,j), res))
		    else res
	    in rl start e end;
    in loop (i+n - 1) end

fun appi f (slice as (a, i, n)) = 
    let fun loop stop = 
	    let	fun lr j = 
		    if j < stop then (f(j, Array.sub(a,j)); lr (j+1)) 
		    else ()
	    in lr i end
    in loop (i+n) end

fun mapi (f : 'a -> 'b) (a, i, n) =
    let
      val newvec : 'a array = Prim.newarray(n)
      fun copy j = 
        if Int.<(j,n)
        then (Prim.arraystore(newvec, j, f(i+j, Array.sub(a,i+j))); copy (j+1))
        else Prim.toVector newvec
    in 
      copy 0 
    end



fun app f sl = appi (f o #2) sl
fun map f sl = mapi (f o #2) sl

fun foldl f init sl = foldli (fn (_, a, x) => f(a, x)) init sl
fun foldr f init sl = foldri (fn (_, a, x) => f(a, x)) init sl

fun all _ _ = raise Fail "ArraySlice.all not implemented"
fun exists _ _ = raise Fail "ArraySlice.exists not implemented"
fun find _ _ = raise Fail "ArraySlice.find not implemented"
fun findi _ _ = raise Fail "ArraySlice.findi not implemented"
fun collate _ _= raise Fail "ArraySlice.collate not implemented"
fun concat _ = raise Fail "ArraySlice.concat not implemented"
fun getItem _ = raise Fail "ArraySlice.getItem not implemented"
fun copy _ = raise Fail "ArraySlice.copy not implemented"
fun copyVec _ = raise Fail "ArraySlice.copyVec not implemented"
fun modifyi _ = raise Fail "ArraySlice.modifyi not implemented"
fun modify _ = raise Fail "ArraySlice.modify not implemented"
end

end