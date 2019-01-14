(*======================================================================*)
(* Standard basis Vector structure.					*)
(* Requires the following primitive array operations:			*)
(*   Prim.newarray : int -> 'a array					*)
(*   Prim.arraylength : 'a array -> int					*)
(*   Prim.arrayload : 'a array * int -> 'a				*)
(*   Prim.arraystore : 'a array * int * 'a -> unit			*)
(*   Prim.fromVector : 'a vector -> 'a array				*)
(*   Prim.toVector : 'a array -> 'a vector				*)
(*   Prim.arraycopy : 'a array*int*'a array*int*int -> unit		*)
(*======================================================================*)
structure Vector :> VECTOR =
struct

local 
  open General List Bool Option Int
  structure P = PrimUtils_ 
  val op= = Prim.=
in

type 'a vector = 'a vector
val maxLen = valOf (Int.maxInt)

(* Primitive stuff first *)
fun length (v : 'a vector) = Prim.arraylength(Prim.fromVector v)

fun sub (v : 'a vector, i : int) = Prim.arrayload (Prim.fromVector v, i)

fun fromList (vs : 'a list) =
  let 
    val n = List.length vs
    val a : 'a array = Prim.newarray(n)
    fun init [] (i : int) = Prim.toVector a
      | init (v::vs) i = (Prim.arraystore(a, i, v); init vs (i+1))
  in 
    init vs 0 
  end

fun tabulate(n : int, f : int -> 'a) = if n<0 then raise General.Size else
  let 
    val a : 'a array = Prim.newarray(n)
    fun init (i : int) = 
      if i = n then Prim.toVector a
      else (Prim.arraystore(a, i, f i); init (i+1))
  in 
    init 0 
  end

fun map (f : 'a -> 'b) (v : 'a vector) =
  let 
    val n = length v
    val a : 'b array = Prim.newarray(n)
    fun init i = 
      if i = n then Prim.toVector a
      else (Prim.arraystore(a, i, f (sub(v,i))); init (i+1))
  in 
    init 0 
  end

fun mapi (f : int*'a -> 'b) (vec : 'a vector) =
    let
      val n = length vec
      val newvec : 'a array = Prim.newarray(n)
      fun copy j = 
        if Int.<(j,n)
        then (Prim.arraystore(newvec, j, f(j, sub(vec,j))); copy (j+1))
        else Prim.toVector newvec
    in 
      copy 0 
    end

fun concat (vecs : 'a vector list) =
    let 
      fun acc [] len      = len
	| acc (v::vs) len = acc vs (length v + len)
      val len = acc vecs 0
      val newvec : 'a array = Prim.newarray(len)
      fun copyall to [] = Prim.toVector newvec
	| copyall to (v1::vr) =
	  let 
            val len1 = length v1
	    fun copy j =
		    if j<len1 then
			(Prim.arraystore(newvec, to+j, sub(v1,j)); copy (j+1))
		    else
			()
	    in copy 0; copyall (to+len1) vr end
    in copyall 0 vecs end

fun foldl f e a = 
  let 
    val n = length a
    fun foldl' (i, result) = 
      if i < n 
      then foldl' (i+1, f (sub(a,i), result))
      else result
  in 
    foldl' (0, e)
  end

fun foldr f e a =
  let
    fun foldr' (i, result) = 
      if i >= 0 
      then foldr' (i-1, f(sub(a,i), result))
      else result
  in
    foldr' (length a - 1, e) 
  end

fun app f a = 
  let 
    val n = length a
    fun app' i = 
      if i < n then (f(sub(a,i)); app' (i+1))
      else ()
  in 
    app' 0 
  end


fun foldli f e a = 
    let fun loop stop =
	    let fun lr j res = 
		if j < stop then lr (j+1) (f(j, sub(a,j), res))
		else res
	    in lr 0 e end
    in loop (length a) end

fun foldri f e a =
    let fun loop start =
	    let fun rl j res = 
		    if j >= 0 then rl (j-1) (f(j, sub(a,j), res))
		    else res
	    in rl start e end;
    in loop (length a - 1) end

fun appi f a =
    let fun loop stop = 
	    let	fun lr j = 
		    if j < stop then (f(j, sub(a,j)); lr (j+1)) 
		    else ()
	    in lr 0 end
    in loop (length a) end

fun update (a,i:int,x) = tabulate (length a, fn j => if i=j then x else sub(a,i))

fun all f a = 
  let fun loop stop =
      let fun lr j = if j=stop then true
                     else if f(sub(a,j)) then lr(j+1) else false
      in lr 0 end
  in loop (length a) 
  end

fun exists f a = 
  let fun loop stop =
      let fun lr j = if j=stop then false
                     else if f(sub(a,j)) then true else lr(j+1)
      in lr 0 end
  in loop (length a) 
  end

fun find f a =
  let fun loop stop =
      let fun lr j = if j=stop then NONE
                     else let val x = sub(a,j)
                          in if f x then SOME x else lr(j+1) end
      in lr 0 end
  in loop (length a) 
  end

fun findi f a =
  let fun loop stop =
      let fun lr j = if j=stop then NONE
                     else let val p = (j,sub(a,j))
                          in if f p then SOME p else lr(j+1) end
      in lr 0 end
  in loop (length a) 
  end

fun collate cmp (s1, s2) =
    let val n1 = length s1 
	and n2 = length s2
	val stop = if n1 < n2 then n1 else n2
	fun h j = (* At this point s1[0..j-1] = s2[0..j-1] *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                             else                 EQUAL
	    else
              let 
                val c1 = sub(s1, j)
                val c2 = sub(s2, j)
              in
		case cmp(c1,c2) of
		    EQUAL   => h (j+1)
                  | result  => result
              end
    in h 0 end

end
end