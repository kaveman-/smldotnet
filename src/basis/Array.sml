(*======================================================================*)
(* Standard basis Array structure                        		*)
(* Requires the following primitive array operations:			*)
(*   Prim.newarray : int -> 'a array					*)
(*   Prim.arraylength : 'a array -> int					*)
(*   Prim.arrayload : 'a array * int -> 'a				*)
(*   Prim.arraystore : 'a array * int * 'a -> unit			*)
(*   Prim.fromVector : 'a vector -> 'a array				*)
(*   Prim.arraycopy : 'a array*int*'a array*int*int -> unit		*)
(*======================================================================*)
structure Array :> ARRAY =
struct

local 
  open Int General List Option
  structure P = PrimUtils_ 
  val op= = Prim.=
in

type 'a array = 'a array
type 'a vector = 'a vector

val maxLen = Option.valOf (Int.maxInt)

fun tabulate(n:int, f : int -> 'a) =
  let 
    val a : 'a array = Prim.newarray(n)
    fun init i = 
      if i = n then a
      else (Prim.arraystore(a, i, f i); init (i+1))
  in 
    init 0 
  end

fun array(n, v : 'a) = 
  let 
    val a : 'a array = Prim.newarray(n)
    fun init i = 
      if i = n then a
      else (Prim.arraystore(a, i, v); init (i+1))
  in 
    init 0 
  end

fun fromList (vs : 'a list) =
    let val n = List.length vs
	val a : 'a array = Prim.newarray(n)
	fun init ([], i) = a
	  | init (v::vs, i) = 
            (Prim.arraystore(a, i, v); init (vs, i+1))
    in 
      init (vs, 0) 
    end

fun length (a : 'a array) = Prim.arraylength(a)

fun sub(a : 'a array, i : int) = Prim.arrayload(a, i)

fun update(a : 'a array, i, v : 'a) = Prim.arraystore(a, i, v)


fun slicelength (a, i, NONE) = 
        if i<0 orelse i>length a then raise Subscript
	else length a - i
  | slicelength (a, i, SOME n) = 
	if i<0 orelse n<0 orelse i+n>length a then raise Subscript
	else n

fun extract (a:'a array,i,nOpt) =
    let 
      val n = slicelength(a,i,nOpt)
      val newvec : 'a array = Prim.newarray(n)
    in
      Prim.arraycopy (a, i, newvec, 0, n);
      Prim.toVector newvec
    end

fun copy {src : 'a array, dst : 'a array, di} =
let
  val len = length src
in
  Prim.arraycopy(src, 0, dst, di, len)
end

fun copyVec {src : 'a vector, dst : 'a array, di} =
let
  val len = Vector.length src
in
  Prim.arraycopy(Prim.fromVector src, 0, dst, di, len)
end

fun vector a = Vector.tabulate (length a, fn i => sub(a,i))

fun foldl f e a = 
    let val stop = length a
	fun lr j res = if j < stop then lr (j+1) (f(sub(a,j), res))
		       else res
    in lr 0 e end

fun foldr f e a =
    let fun rl j res = if j >= 0 then rl (j-1) (f(sub(a,j), res))
		       else res
    in rl (length a - 1) e end

fun modify f a = 
    let val stop = length a
	fun lr j = if j < stop then (update(a, j, f(sub(a, j))); lr (j+1))
		   else ()
    in lr 0 end

fun app f a = 
    let val stop = length a
	fun lr j = if j < stop then (f(sub(a, j)); lr (j+1))
		   else ()
    in lr 0 end

fun sliceend (a, i, NONE) = 
        if i<0 orelse i>length a then raise Subscript
	else length a
  | sliceend (a, i, SOME n) = 
	if i<0 orelse n<0 orelse i+n>length a then raise Subscript
	else i+n;

fun foldli f e a =
    let fun loop stop =
	    let fun lr j res = 
		if j < stop then lr (j+1) (f(j, sub(a, j), res))
		else res
	    in lr 0 e end
    in loop (length a) end;

fun foldri f e a =
    let fun loop start =
	    let fun rl j res = 
		    if j >= 0 then rl (j-1) (f(j, sub(a, j), res))
		    else res
	    in rl start e end;
    in loop (length a - 1) end

fun modifyi f a = 
    let fun loop stop =
	    let fun lr j = 
		if j < stop then (update(a, j, f(j, sub(a, j))); lr (j+1))
		else ()
	    in lr 0 end
    in loop (length a) end;

fun appi f a =
    let fun loop stop = 
	    let	fun lr j = 
		    if j < stop then (f(j, sub(a, j)); lr (j+1)) 
		    else ()
	    in lr 0 end
    in loop (length a) end;

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

