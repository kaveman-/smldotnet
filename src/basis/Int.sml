(*======================================================================*)
(* Implementation of 32-bit integers (the default integer type)		*)
(* All target-dependent ops have been factored into PrimUtils_.		*)
(* We depart from the standard in not raising Overflow.                 *)
(*======================================================================*)
structure Int : INTEGER =
struct

local 
  open General  Bool  Option 
  val op= = Prim.=
in

type int = int

val precision = SOME 32
val maxInt    = SOME 2147483647
val minInt    = SOME ~2147483648

fun toInt   i = i : int
fun fromInt i = i : int

open PrimUtils_.Int

fun sign i = if i < 0 then ~1 else if i > 0 then 1 else 0

fun compare (x, y) = 
  if x < y then LESS
  else if x > y then GREATER 
  else EQUAL



(* Thanks to George for these *)
(*@TODO: fix div*)
(*@BUG: produces the wrong result if x-y overflows (eg [minInt div 2] is wrong) 
fun x div y =
   let
      val signx=if x<0 then ~1 else 1
      val signy=if y<0 then ~1 else 1
   in
      if signx=signy then quot(x,y)
      else 
         quot(x-y+signy,y)
   end
*)
fun x div y =
   let
      val signx=if x<0 then ~1 else 1
      val signy=if y<0 then ~1 else 1
   in
      if signx=signy then quot(x,y)
      else if signx > signy then
	        quot(x-y+signy,y)
	   else quot(x+signy,y)-1
   end

(*@TODO: test at boundaries *)
fun x mod y=
   let
      val signx=if x<0 then ~1 else 1
      val signy=if y<0 then ~1 else 1
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

val fromString = Utils_.scanString (scan StringCvt.DEC)

end (* of local open General Bool Option *)

end (* of struct *)
