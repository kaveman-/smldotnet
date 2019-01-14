(*======================================================================*)
(* CharVector structure, whose type must be identical to String.string.	*)
(* Pure ML!                                                             *)
(*======================================================================*)
structure CharVector :> MONO_VECTOR 
  where type elem = Char.char and type vector = String.string 
= struct

type elem = Char.char 
type vector = String.string

val maxLen = Option.valOf (Int.maxInt)

local 
  open Int 
  structure P = PrimUtils_ (*@HACK *)
  val op= = Prim.=
in

fun tabulate (n,f) = if n<0 then raise General.Size else
let
  val sb = StringBuffer_.emptyWith n
  fun tr j = if j = n then StringBuffer_.toString sb
             else (StringBuffer_.appendChar(sb, f j); tr (j+1))
in
  tr 0
end
  
val length = String.size
val fromList = String.implode
val extract = String.extract
val concat = String.concat
val sub = String.sub

fun foldl f e a = 
    let	val stop = length a
	fun lr j res = if j < stop then lr (j+1) (f(sub(a, j), res))
		       else res
    in lr 0 e end

fun foldr f e a =
    let	fun rl j res = if j >= 0 then rl (j-1) (f(sub(a, j), res))
		       else res
    in rl (length a - 1) e end

fun app f a = 
    let val stop = length a
	fun lr j = if j < stop then (f(sub(a, j)); lr (j+1))
		   else ()
    in lr 0 end

fun sliceend (a, i, Option.NONE) = 
        if i<0 orelse i>length a then raise General.Subscript
	else length a
  | sliceend (a, i, Option.SOME n) = 
	if i<0 orelse n<0 orelse i+n>length a then raise General.Subscript
	else i+n

fun foldli f e a =
    let fun loop stop =
	    let fun lr j res = 
		if j < stop then lr (j+1) (f(j, sub(a, j), res))
		else res
	    in lr 0 e end
    in loop (length a) end

fun foldri f e a =
    let fun loop start =
	    let fun rl j res = 
		    if j >= 0 then rl (j-1) (f(j, sub(a, j), res))
		    else res
	    in rl start e end
    in loop (length a - 1) end

fun appi f a =
    let fun loop stop = 
	    let	fun lr j = 
		    if j < stop then (f(j, sub(a, j)); lr (j+1)) 
		    else ()
	    in lr 0 end
    in loop (length a) end

fun map f v =
let
  val n = length v
  val sb = StringBuffer_.emptyWith n
  fun tr j = if j = n then StringBuffer_.toString sb
             else (StringBuffer_.appendChar(sb, f(sub(v, j))); tr (j+1))
in
  tr 0
end
  
fun mapi f  a =
let
  val stop = length a
  val sb = StringBuffer_.emptyWith stop
  fun tr j = if j = stop then StringBuffer_.toString sb
             else (StringBuffer_.appendChar(sb, f(j, sub(a, j))); tr (j+1))
in
  tr 0
end

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
            if j = stop then if n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                                            else EQUAL
            else
                let 
                    val c1 = sub(s1, j)
                    val c2 = sub(s2, j)
                in
                    case cmp(c1,c2) of
                        EQUAL   => h (j+1)
                      | result  => result
                end
    in 
        h 0 
    end
        
end

end
