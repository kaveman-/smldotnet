(*======================================================================*)
(* Implementation of 64-bit integers (the largest fixed integer type)	*)
(* All target-dependent ops have been factored into PrimUtils_.		*)
(* We depart from the standard in not raising Overflow.                 *)
(*======================================================================*)
structure FixedInt : INTEGER =
struct

local 
  open General Bool Option 
  val op= = Prim.=
in

open PrimUtils_.FixedInt

val precision = SOME (64 : int)
val maxInt    = SOME ( 9223372036854775807 : int)
val minInt    = SOME (~9223372036854775808 : int)

fun toLarge (x:int) = x
fun fromLarge (x:int) = x

fun sign i = if i < 0 then ~1 else if i > 0 then 1 else 0

fun compare (x, y) = 
  if x < y then LESS
  else if x > y then GREATER 
  else EQUAL

(* Thanks to George for these *)
(*@TODO: fix div *)
(*@BUG: produces the wrong result if x-y overflows (eg [minInt div 2] is wrong)fun x div y=
   let
      val signx=if x<0 then ~1 else 1:int
      val signy=if y<0 then ~1 else 1:int
   in
      if signx=signy then quot(x,y)
      else 
         quot(x-y+signy,y)
   end
*)
fun x div y =
   let
      val signx=if x<0 then ~1 else 1:int
      val signy=if y<0 then ~1 else 1:int
   in
      if signx=signy then quot(x,y)
      else if signx > signy then
	        quot(x-y+signy,y)
	   else quot(x+signy,y)-1
   end


fun x mod y=
   let
      val signx=if x<0 then ~1 else 1:int
      val signy=if y<0 then ~1 else 1:int
   in
      if signx=signy then rem(x,y)
      else
         y-signy + rem(x+signy,y)
   end
fun abs (x:int) = if x < 0 then ~x else x
fun min (x:int, y) = if x < y then x else y
fun max (x:int, y) = if x > y then x else y

fun sameSign (i, j) = sign i = sign j


val fmt = Utils_.fmt {lt = op <,
		     mod = abs o rem,
		     div = op quot, 
		     fromInt = fromInt,
		     toInt = toInt,
		     zero = 0} 



fun scan radix getc src =
    Utils_.scanInteger {fromInt = fromInt,
		   quot = quot, 
		   maxInt = maxInt,
		   minInt = minInt,
		   + = op +,
		   - = op -,
		   ~ = ~,
		   * = op *,
		   < = op <, 
		   > = op >}  
                   radix getc src 


val toString = fmt (StringCvt.DEC)

val fromString = StringCvt.scanString (scan StringCvt.DEC)

end (* of local open General Bool Option *)

end (* of struct *)
